{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleInstances,
  MultiParamTypeClasses, UndecidableInstances, TypeFamilies,
  FlexibleContexts, RankNTypes,CPP,TemplateHaskell, StandaloneDeriving #-}
-- | the utility monad and related functions, taking care of the HTTP, JSON, etc.
module Web.MangoPay.Monad where

import Web.MangoPay.Types

import Control.Applicative
import Control.Monad (MonadPlus, liftM, void, join)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control ( MonadTransControl(..), MonadBaseControl(..)
                                   , ComposeSt, defaultLiftBaseWith
                                   , defaultRestoreM )
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Default
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Exception.Lifted as L

import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Aeson (json, fromJSON, toJSON, Result(..)
                  ,FromJSON, ToJSON,encode, Object, Value(..))
import Data.Conduit.Attoparsec (sinkParser, ParseError)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T (Text)
import Data.Maybe (fromMaybe)
import Data.CaseInsensitive (CI)

import Control.Monad.Logger

#if DEBUG
import Data.Conduit.Binary (sinkHandle)
import System.IO (stdout)
import Data.Conduit.Internal (zipSinks)
import Control.Monad.IO.Class (liftIO)
#endif

-- | put our constraints in one synonym
type MPUsableMonad m=(MonadBaseControl IO m, R.MonadResource m, MonadLogger m)

-- | the mangopay monad transformer
-- this encapsulates the data necessary to pass the app credentials, etc
newtype MangoPayT m a = Mp { unIs :: ReaderT MpData m a }
    deriving ( Functor, Applicative, Alternative, Monad
             , MonadFix, MonadPlus, MonadIO, MonadTrans
             , R.MonadThrow )

deriving instance R.MonadResource m => R.MonadResource (MangoPayT m)

instance MonadBase b m => MonadBase b (MangoPayT m) where
    liftBase = lift . liftBase

instance MonadTransControl MangoPayT where
    newtype StT MangoPayT a = MpStT { unMpStT :: StT (ReaderT MpData) a }
    liftWith f = Mp $ liftWith (\run -> f (liftM MpStT . run . unIs))
    restoreT = Mp . restoreT . liftM unMpStT

instance MonadBaseControl b m => MonadBaseControl b (MangoPayT m) where
    newtype StM (MangoPayT m) a = StMT {unStMT :: ComposeSt MangoPayT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM = defaultRestoreM unStMT

instance (MonadLogger m) => MonadLogger (MangoPayT m) where
    monadLoggerLog loc src lvl msg=lift $ monadLoggerLog loc src lvl msg

-- | Run a computation in the 'MangoPayT' monad transformer with
-- your credentials.
runMangoPayT :: Credentials -- ^ Your app's credentials.
             -> H.Manager -- ^ Connection manager (see 'H.withManager').
             -> AccessPoint
             -> MangoPayT m a -- ^ the action to run
             -> m a -- ^ the result
runMangoPayT creds manager ap (Mp act) =
    runReaderT act (MpData creds manager ap)

-- | Get the user's credentials.
getCreds :: Monad m => MangoPayT m Credentials
getCreds = mpCreds `liftM` Mp ask

-- | Get the MangoPay host
getHost :: Monad m => MangoPayT m ByteString
getHost = (getAccessPointURL . mpAccessPoint) `liftM` Mp ask


-- | Build a POST request to MangoPay.
getPostRequest
  :: (Monad m, MonadIO m, HT.QueryLike q)
  => ByteString
     -- ^ The URL path.
  -> Maybe AccessToken
  -> q
     -- ^ The query parameters.
  -> MangoPayT m H.Request
     -- ^ The properly configured request.
getPostRequest path mat query = do
  let b = HT.renderQuery False $ HT.toQuery query
  getBasicRequest HT.methodPost path $ \r ->
    r { H.requestHeaders =
          ("Content-Type", "application/x-www-form-urlencoded") :
          [("Authorization", at) | Just (AccessToken at) <- [mat]]
      , H.requestBody = H.RequestBodyBS b }


-- | Build a GET request to MangoPay.
getGetRequest
  :: (Monad m, MonadIO m, HT.QueryLike q)
  => ByteString
     -- ^ The URL path.
  -> Maybe AccessToken
  -> q
     -- ^ The query parameters.
  -> MangoPayT m H.Request
     -- ^ The properly configured request.
getGetRequest path mat query=do
  let qs = HT.renderQuery True $ HT.toQuery query
  getBasicRequest HT.methodGet path $ \r ->
    r { H.queryString = qs
      , H.requestHeaders = getJSONHeaders mat
      }


-- | (Internal) Build a basic request.
getBasicRequest
  :: MonadIO m
  => HT.Method
  -> ByteString
  -> (H.Request -> H.Request)
  -> MangoPayT m H.Request
getBasicRequest method path addRest = do
  host <- getHost
  let req1 =
        def
          { H.secure = True
          , H.host   = host
          , H.port   = 443
          , H.path   = path
          , H.method = method
          }
      req2 = addRest req1
#if DEBUG
  liftIO $ do
    print req2
    putStrLn $ "^--> " ++
      case H.requestBody req2 of
        H.RequestBodyLBS lbs -> "RequestBodyLBS " ++ show lbs
        H.RequestBodyBS  bs  -> "RequestBodyBS "  ++ show bs
        H.RequestBodyBuilder s _ -> "RequestBodyBuilder " ++ show s ++ " <Builder>"
        H.RequestBodyStream  s _ -> "RequestBodyStream "  ++ show s ++ " <GivesPopper ()>"
        H.RequestBodyStreamChunked _ -> "RequestBodyStreamChunked <GivesPopper ()>"
#endif
  return req2


-- | build a delete request  to MangoPay
getDeleteRequest :: (Monad m,MonadIO m,HT.QueryLike q) => ByteString -- ^ the url path
  -> Maybe AccessToken
  -> q -- ^ the query parameters
  -> MangoPayT m H.Request -- ^ the properly configured request
getDeleteRequest path mat query=do
  get<-getGetRequest path mat query
  return $ get {H.method=HT.methodDelete}

-- | get the url to use for our clientId
getClientURL :: (Monad m)=> ByteString -- ^ the url path
        -> MangoPayT m ByteString  -- ^ the URL
getClientURL path=do
        cid<- liftM clientIdBS getCreds
        return $ BS.concat ["/v2/",cid,path]

-- | get the url to use for our clientId
getClientURLMultiple :: (Monad m)=> [T.Text] -- ^ the url components
        -> MangoPayT m ByteString  -- ^ the URL
getClientURLMultiple path=do
        cid<- liftM clientIdBS getCreds
        return $ BS.concat $ ["/v2/",cid] ++ map TE.encodeUtf8 path

-- | build a URL for a get operation with a single query
getQueryURL :: (Monad m,HT.QueryLike q) => ByteString -- ^ the url path
  -> q -- ^ the query parameters
  -> MangoPayT m ByteString  -- ^ the URL
getQueryURL path query=do
  host<-getHost
  return $ BS.concat ["https://",host,path,HT.renderQuery True  $ HT.toQuery query]

-- | perform a HTTP request and deal with the JSON result
-- The logic for errors is as follows: we have several cases:
-- If the HTTP request return OK and we can parse the proper result, we return it.
-- If we can't parse it into the data type we expect, we throw a MpJSONException: the server returned ok but we can't parse the result.
-- If we get an HTTP error code, we try to parse the result and send the proper exception: we have encountered probably a normal error, when the user has filled in incorrect data, etc.
-- If we can't even parse the result as JSON or if we can't understand the JSON error message, we throw an MpHttpException.
mpReq :: forall b (m :: * -> *) wrappedErr c .
                    (MPUsableMonad m,FromJSON b,FromJSON wrappedErr) =>
                    H.Request
                    -> (wrappedErr -> MpError) -- ^ extract the error from the JSON
                    -> (HT.ResponseHeaders -> b -> c)
                    -> MangoPayT m c
mpReq req extractError addHeaders=do
   -- we check the status ourselves
  let req' = req { H.checkStatus = \_ _ _ -> Nothing }
  mgr<-getManager
  res<-H.http req' mgr
  let status = H.responseStatus res
      headers = H.responseHeaders res
      cookies = H.responseCookieJar res
      ok=isOkay status
      err=H.StatusCodeException status headers cookies
  mpres<-L.catch (do
#if DEBUG
    liftIO $ BSC.putStrLn ""
    liftIO $ print $ show req'
    (value,_)<-H.responseBody res C.$$+- zipSinks (sinkParser json) (sinkHandle stdout)
    liftIO $ BSC.putStrLn ""
    liftIO $ print headers
#else
    value<-H.responseBody res C.$$+- sinkParser json
#endif
    if ok
      then
          -- parse response as the expected value
          case fromJSON value of
            Success ot->return $ Right (value,addHeaders headers ot)
            Error jerr->return $ Left $ MpJSONException jerr -- got an ok response we couldn't parse
      else
          -- parse response as an error
          case fromJSON value of
            Success ise-> return $ Left $ MpAppException $ extractError ise
            _ -> return $ Left $  MpHttpException err $ Just value -- we can't even parse the error, throw the HTTP error inside our error type, but keep the JSON in case a human can make sens of it
    ) (\(_::ParseError)->return $ Left $  MpHttpException err Nothing) -- the error body wasn't even json, throw the HTTP error inside our error type
  let cr=CallRecord req' mpres
  -- log call
  $(logCall) cr
  recordResult cr

-- | get a JSON response from a request to MangoPay
-- MangoPay returns either a result, or an error
getJSONResponse :: forall (m :: * -> *) v.
                                 (MPUsableMonad m,FromJSON v) =>
                                 H.Request
                                 -> MangoPayT
                                      m v
getJSONResponse req=mpReq req id (const id)

-- | get a PagedList from the JSON items
getJSONList:: forall (m :: * -> *) v.
                                 (MPUsableMonad m,FromJSON v) =>
                                 H.Request
                                 -> MangoPayT
                                      m (PagedList v)
getJSONList req=mpReq req id buildList

-- | build a PagedList from the headers information
buildList :: HT.ResponseHeaders -> [b] -> PagedList b
buildList headers items=let
    cnt=fromMaybe (fromIntegral $ length items) $  getI "X-Number-Of-Items" -- yes, doc is wrong (they forgot the s)
    pgs=fromMaybe 1 $ getI "X-Number-Of-Pages"
    in PagedList items cnt pgs
    where
      getI :: CI ByteString -> Maybe Integer
      getI =join . fmap ((maybeRead :: String -> Maybe Integer). BSC.unpack) . findAssoc headers

-- | get all items, hiding the pagination system
getAll ::  (MPUsableMonad m,FromJSON v) =>
  (Maybe Pagination -> AccessToken -> MangoPayT m (PagedList v)) -> AccessToken ->
  MangoPayT m [v]
getAll f at=readAll 1 []
  where
    readAll p accum=do
        retL<-f (Just $ Pagination p 100) at
        let dts=accum ++ plData retL
        if plPageCount retL > p
                then readAll (p + 1) dts
                else return dts

-- | get the headers necessary for a JSON call
getJSONHeaders ::  Maybe AccessToken -> HT.RequestHeaders
getJSONHeaders mat=  ("content-type", "application/json") :
  case mat of
      Just (AccessToken at) -> [("Authorization", at)]
      _ -> []


-- | send JSON via post, get JSON back
postExchange :: forall (m :: * -> *) v p.
                 (MPUsableMonad m,FromJSON v,ToJSON p) =>
                 ByteString
                 -> Maybe AccessToken
                 -> p
                 -> MangoPayT
                      m v
postExchange=jsonExchange HT.methodPost

-- | send JSON via post, get JSON back
putExchange :: forall (m :: * -> *) v p.
                 (MPUsableMonad m,FromJSON v,ToJSON p) =>
                 ByteString
                 -> Maybe AccessToken
                 -> p
                 -> MangoPayT
                      m v
putExchange=jsonExchange HT.methodPut

-- | send JSON, get JSON back
jsonExchange :: forall (m :: * -> *) v p.
                 (MPUsableMonad m,FromJSON v,ToJSON p) =>
                 HT.Method
                 -> ByteString
                 -> Maybe AccessToken
                 -> p
                 -> MangoPayT
                      m v
jsonExchange meth path mat p= getJSONRequest meth path mat p >>= getJSONResponse


-- | Get JSON request.
getJSONRequest
  :: (MPUsableMonad m, ToJSON p)
  => HT.Method
  -> ByteString
  -> Maybe AccessToken
  -> p
  ->  MangoPayT m H.Request
getJSONRequest method path mat p = do
  getBasicRequest method path $ \r ->
    r { H.requestHeaders = getJSONHeaders mat
      , H.requestBody = H.RequestBodyLBS $ encode p
      }


-- | post JSON content and ignore the reply
postNoReply :: forall (m :: * -> *) p.
                 (MPUsableMonad m,ToJSON p) =>
                  ByteString
                 -> Maybe AccessToken
                 -> p
                 -> MangoPayT
                      m ()
postNoReply path mat p= do
  req<- getJSONRequest HT.methodPost path mat p
  mgr<-getManager
  void $ H.http req mgr

-- | Get the 'H.Manager'.
getManager :: Monad m => MangoPayT m H.Manager
getManager = mpManager `liftM` Mp ask

-- | Run a 'ResourceT' inside a 'MangoPayT'.
runResourceInMp :: (MPUsableMonad m) =>
                   MangoPayT (R.ResourceT m) a
                -> MangoPayT m a
runResourceInMp (Mp inner) = Mp $ ask >>= lift . R.runResourceT . runReaderT inner

-- | Transform the computation inside a 'MangoPayT'.
mapMangoPayT :: (m a -> n b) -> MangoPayT m a -> MangoPayT n b
mapMangoPayT f = Mp . mapReaderT f . unIs

-- | the data kept through the computations
data MpData = MpData {
        mpCreds::Credentials -- ^ app credentials
        ,mpManager::H.Manager -- ^ HTTP connection manager
        ,mpAccessPoint:: AccessPoint -- ^ access point
        }
        deriving (Typeable)

-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
  let sc = HT.statusCode status
  in 200 <= sc && sc < 300


-- | helper function to create an entity using POST
createGeneric :: (MPUsableMonad m, FromJSON a, ToJSON a) =>
  ByteString -> a -> AccessToken -> MangoPayT m a
createGeneric path x at = do
        url<-getClientURL path
        postExchange url (Just at) x


-- | helper function to create an entity using PUT
modifyGeneric :: (MPUsableMonad m, FromJSON a, ToJSON a) =>
  T.Text -> a -> (a -> Maybe T.Text) -> AccessToken -> MangoPayT m a
modifyGeneric = modifyGGeneric Nothing


-- | even more generic version of 'modifyGeneric' that can put an
-- arbitrary json value
modifyGGeneric :: (MPUsableMonad m, FromJSON a, ToJSON a) =>
  Maybe (Object -> Object) -> T.Text -> a -> (a -> Maybe T.Text) ->
    AccessToken -> MangoPayT m a
modifyGGeneric mf path x fid at =
        case fid x of
          Nothing -> error $ show $
            "Web.MangoPay.Users.modifyGGeneric : Nothing (" <> path <> ")"
          Just i -> do
            url<-getClientURLMultiple [path ,i]
            case mf of
              Nothing -> putExchange url (Just at) x
              Just f -> do
                let Object o = toJSON x
                putExchange url (Just at) $ f o


-- | helper function to fetch an entity from its id
fetchGeneric :: (MPUsableMonad m, FromJSON a) =>
  T.Text -> T.Text -> AccessToken -> MangoPayT m a
fetchGeneric path xid at = do
        url<-getClientURLMultiple [path ,xid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req


-- | helper function to fetch paginated lists of an entity
genericList :: (MPUsableMonad m, FromJSON a) =>
  [T.Text] -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList a)
genericList = genericListExtra []

-- | helper function to fetch paginated lists of an entity, with extra parameters
genericListExtra :: (MPUsableMonad m, FromJSON a) =>
  [(ByteString,Maybe ByteString)] -> [T.Text] -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList a)
genericListExtra extra path mp at = do
        url <- getClientURLMultiple path
        req <- getGetRequest url (Just at) (extra ++ (paginationAttributes mp))
        getJSONList req
