{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell, PatternGuards #-}
-- | useful types and simple accessor functions
module Web.MangoPay.Types where


import Control.Applicative
import Control.Exception.Lifted (Exception, throwIO)
import Control.Monad.Base (MonadBase)
import Data.Text as T hiding (singleton)
import Data.Text.Read as T
import Data.Typeable (Typeable)
import Data.ByteString  as BS (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Aeson
import Data.Default

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (listToMaybe)
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Control.Monad.Logger
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy.Builder (fromText, toLazyText, singleton)
import Data.Monoid ((<>), mempty)
import Data.Text.Lazy (toStrict)
import Data.String (fromString, IsString)
import qualified Data.Vector as V (length)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qLocation)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BS (toStrict)

import Data.ISO3166_CountryCodes (CountryCode)

-- | the MangoPay access point
data AccessPoint = Sandbox | Production | Custom ByteString
        deriving (Show,Read,Eq,Ord,Typeable)

-- | get the real url for the given access point
getAccessPointURL :: AccessPoint -> ByteString
getAccessPointURL Sandbox="api.sandbox.mangopay.com"
getAccessPointURL Production="api.mangopay.com"
getAccessPointURL (Custom bs)=bs

-- | the app credentials
data Credentials = Credentials {
  cClientID :: Text -- ^ client id
  ,cName :: Text -- ^ the name
  ,cEmail :: Text -- ^ the email
  ,cClientSecret :: Maybe Text -- ^ client secret, maybe be Nothing if we haven't generated it
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Credentials  where
    toJSON c=object ["ClientId" .= cClientID c, "Name" .= cName c , "Email" .= cEmail c,"Passphrase" .= cClientSecret c]

-- | from json as per MangoPay format
instance FromJSON Credentials where
    parseJSON (Object v) =Credentials <$>
                         v .: "ClientId" <*>
                         v .: "Name" <*>
                         v .: "Email" <*>
                         v .: "Passphrase"
    parseJSON _= fail "Credentials"

-- | get client id in ByteString form
clientIDBS :: Credentials -> ByteString
clientIDBS=TE.encodeUtf8 . cClientID


-- | the access token is simply a Text
newtype AccessToken=AccessToken ByteString
    deriving (Eq, Ord, Read, Show, Typeable)


-- | the oauth token returned after authentication
data OAuthToken = OAuthToken {
  oaAccessToken :: Text -- ^ the access token
  ,oaTokenType :: Text -- ^ the token type
  ,oaExpires :: Int -- ^ expiration
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON OAuthToken  where
    toJSON oa=object ["access_token" .= oaAccessToken oa, "token_type" .= oaTokenType oa, "expires_in" .= oaExpires oa]

-- | from json as per MangoPay format
instance FromJSON OAuthToken where
    parseJSON (Object v) =OAuthToken <$>
                         v .: "access_token" <*>
                         v .: "token_type" <*>
                         v .: "expires_in"
    parseJSON _= fail "OAuthToken"

-- | build the access token from the OAuthToken
toAccessToken ::  OAuthToken -> AccessToken
toAccessToken  oa=AccessToken $ TE.encodeUtf8 $ T.concat [oaTokenType oa, " ",oaAccessToken oa]

-- | an exception that a call to MangoPay may throw
data MpException = MpJSONException String -- ^ JSON parsingError
  | MpAppException MpError -- ^ application exception
  | MpHttpException H.HttpException (Maybe Value) -- ^ HTTP level exception, maybe with some JSON payload
  deriving (Show,Typeable)

-- | make our exception type a normal exception
instance Exception MpException


-- | an error returned to us by MangoPay
data MpError = MpError {
  igeID :: Text
  ,igeType :: Text
  ,igeMessage :: Text
  ,igeDate :: Maybe POSIXTime
  }
  deriving (Show,Eq,Ord,Typeable)



-- | from json as per MangoPay format
instance FromJSON MpError where
    parseJSON (Object v) = MpError <$>
                         v .: "Id" <*>
                         v .: "Type" <*>
                         v .: "Message" <*>
                         v .: "Date"
    parseJSON _= fail "MpError"

-- | from json as per MangoPay format
instance FromJSON POSIXTime where
    parseJSON n@(Number _)=(fromIntegral . (round::Double -> Integer)) <$> parseJSON n
    parseJSON _ = fail "POSIXTime"

-- | to json as per MangoPay format
instance ToJSON POSIXTime  where
    toJSON pt=toJSON (round pt :: Integer)

-- | to json as per MangoPay format
instance ToJSON CountryCode where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON CountryCode where
  parseJSON (String s)
    | ((a,_):_)<-reads $ unpack s=pure a
  parseJSON _ =fail "CountryCode"


-- | Pagination info for searches
-- <http://docs.mangopay.com/api-references/pagination/>
data Pagination = Pagination {
        pPage :: Integer
        ,pPerPage :: Integer
        }
        deriving (Show,Read,Eq,Ord,Typeable)

instance Default Pagination where
        def=Pagination 1 10

-- | get pagination attributes for query
paginationAttributes :: Maybe Pagination -> [(ByteString,Maybe ByteString)]
paginationAttributes (Just p)=["page" ?+ pPage p, "per_page" ?+ pPerPage p]
paginationAttributes _=[]

data PagedList a= PagedList {
  plData :: [a]
  ,plItemCount :: Integer
  ,plPageCount :: Integer
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | ID of a card
type CardID=Text

-- | alias for Currency
type Currency=Text

-- | the expiration date of a card
data CardExpiration = CardExpiration {
  ceMonth :: Int
  ,ceYear :: Int
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | read Card Expiration from text representation (MMYY)
readCardExpiration :: T.Reader CardExpiration
readCardExpiration t |
  4 == T.length t,
  (m,y)<-T.splitAt 2 t=do
    im<-T.decimal m
    iy<-T.decimal y
    return (CardExpiration (fst im) (fst iy), "")
readCardExpiration _ =Left "Incorrect length"

-- | write card expiration
writeCardExpiration :: CardExpiration -> Text
writeCardExpiration (CardExpiration m y)=let
  -- yes I know about text-format, but I don't think performance is that critical here to warrant another dependency
  sm=printf "%02d" $ checkRng m
  sy=printf "%02d" $ checkRng y
  in T.concat [pack sm, pack sy]
  where
    -- | check range fits in two digits
    checkRng :: Int -> Int
    checkRng i=if i > 99 then i `mod` 100 else i

-- | read Card Expiration from JSON string (MMYY)
instance FromJSON CardExpiration where
  parseJSON (String s) |
    Right (ce,"")<- readCardExpiration s=pure ce
  parseJSON _=fail "CardExpiration"

instance IsString CardExpiration where
  fromString s
    | Right (ce,"")<-readCardExpiration $ fromString s=ce
  fromString _=error "CardExpiration"

-- | a structure holding the information of an API call
data CallRecord a = CallRecord {
    crReq :: H.Request -- ^ the request to MangoPay
    ,crResult :: Either MpException (Value,a) -- ^ the error or the JSON value and parsed result
  }

-- | which level should we log the call
recordLogLevel :: CallRecord a-> LogLevel
recordLogLevel cr
  | HT.methodGet == H.method (crReq cr)=LevelDebug
  | otherwise = LevelInfo

-- | the log message from a call
recordLogMessage :: CallRecord a-> Text
recordLogMessage (CallRecord req res)=let
  -- we log the method
  methB=fromString $ show $ H.method req
  -- we log the uri path
  pathB=fromText $ TE.decodeUtf8 $ H.path req
  -- log the query string if any
  qsB=fromText $ TE.decodeUtf8 $ H.queryString req
  postB=if H.method req==HT.methodPost 
    then case H.requestBody req of
      (H.RequestBodyBS b)->fromText (TE.decodeUtf8 b) <> " -> "
      (H.RequestBodyLBS b)->fromText $ TE.decodeUtf8 $ BS.toStrict b <> " -> "
      _->mempty
    else mempty
  resB=case res of
    -- log error
    Left e->fromString $ show e
    Right (v,_)->case v of
      -- we have a list, just log the number of results to avoid polluting the log with too much info
      Array arr->fromString (show $ V.length arr) <> " values"
      -- we have a simple value we can log it
      _->encodeToTextBuilder v
  in toStrict . toLazyText $ methB <> singleton ' ' <> pathB <> qsB <> ": " <> postB <> resB

-- | the result
-- if we have a proper result we return it
-- if we have an error we throw it
recordResult :: MonadBase IO m => CallRecord a -> m a
recordResult (CallRecord _ (Left err))=throwIO err
recordResult (CallRecord _ (Right (_,a)))=return a

-- | log a CallRecord
-- MonadLogger doesn't expose a function with a dynamic log level...
logCall :: Q Exp
logCall = [|\a -> monadLoggerLog $(qLocation >>= liftLoc) "mangopay" (recordLogLevel a) (recordLogMessage a)|]

-- | simple class used to hide the serialization of parameters and simplify the calling code
class ToHtQuery a where
  (?+) :: ByteString -> a -> (ByteString,Maybe ByteString)

instance ToHtQuery Double where
  n ?+ d=n ?+ show d

instance ToHtQuery (Maybe Double) where
  n ?+ d=n ?+ fmap show d

instance ToHtQuery Integer where
  n ?+ d=n ?+ show d

instance ToHtQuery (Maybe Integer) where
  n ?+ d=n ?+ fmap show d

instance ToHtQuery (Maybe POSIXTime) where
  n ?+ d=n ?+ fmap (show . (round :: POSIXTime -> Integer)) d

instance ToHtQuery (Maybe T.Text) where
  n ?+ d=(n,fmap TE.encodeUtf8 d)

instance ToHtQuery T.Text where
  n ?+ d=(n,Just $ TE.encodeUtf8 d)


instance ToHtQuery (Maybe String) where
  n ?+ d=(n,fmap UTF8.fromString d)

instance ToHtQuery String where
  n ?+ d=(n,Just $ UTF8.fromString d)

-- | find in assoc list
findAssoc :: Eq a=> [(a,b)] -> a -> Maybe b
findAssoc xs n=listToMaybe $ Prelude.map snd $ Prelude.filter ((n==) . fst) xs

-- | read an object or return Nothing
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
