{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             OverloadedStrings, PatternGuards, RankNTypes, TemplateHaskell #-}
module Web.MangoPay.TestUtils
  ( -- * Initialization
    withMangoPayTestUtils

    -- * Test helpers
  , testCardInfo1
  , testMP
  , testEventTypes
  , testEventTypes'

    -- ** Credit card registration
  , unsafeFullRegistration
  , unsafeRegisterCard

    -- ** Tear down
  , ensureNoEvents

    -- ** Bug workarounds
  , withWorkaroundManager
  , newWorkaroundManager
  ) where

import Blaze.ByteString.Builder (copyByteString)
import Control.Applicative
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (void, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logDebugS, logWarnS)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit (($$+-))
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Typeable (Typeable)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit

import Web.MangoPay

import qualified Control.Concurrent.Async as AS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as H
import qualified Data.Aeson as A
import qualified Data.Conduit.List as EL
import qualified Data.IORef as I
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Network.Connection as Conn
import qualified Network.HTTP.Types as HT
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX
import qualified Network.Wai as W
import qualified System.X509 as X509


----------------------------------------------------------------------


-- | A test card.
testCardInfo1 :: CardInfo
testCardInfo1 = CardInfo "4970100000000154" "1220" "123"


-- | Test MangoPay API call, logging in with the client
-- credentials.  Expects a file called @client.test.conf@
-- containing the JSON of client credentials in the current
-- directory.
testMP :: (AccessToken -> MangoPayT (LoggingT (ResourceT IO)) b) -> IO b
testMP f = do
  ior <- I.readIORef testState
  let mgr  = tsManager ior
      at   = tsAccessToken ior
      cred = tsCredentials ior
  runResourceT $ runStdoutLoggingT $ runMangoPayT cred mgr Sandbox $ f at


-- | Test an event, checking the resource id and the event type.
testEvent :: Maybe Text -> EventType -> Event -> Bool
testEvent tid et evt =
  tid == Just (eResourceId evt) &&
  et  == eEventType evt

-- | Run the given test, then check that we've received events
-- of the given types which reference resource id returned by the
-- test.
testEventTypes :: [EventType] -> IO (Maybe Text) -> Assertion
testEventTypes evtTs = void . testEventTypes' evtTs


-- | How long to wait for an event, in seconds.
secondsToWaitForEvent :: Integer
secondsToWaitForEvent = 20


-- | Same as 'testEventTypes', but also return the resource id.
testEventTypes' :: [EventType] -> IO (Maybe Text) -> IO (Maybe Text)
testEventTypes' evtTs ops = do
  res <- liftM tsReceivedEvents $ I.readIORef testState
  a <- ops
  er <- waitForEvent res ((,) a <$> evtTs) secondsToWaitForEvent
  assertEqual "testEventTypes'" EventsOK er
  return a


--------------------------------------------------------------------------------


-- | Result of waiting for an event.
data EventResult =
    Timeout [(Maybe Text, EventType)] -- ^ Didn't receive all expected events within the timeout.
  | EventsOK -- ^ OK: everything expected received, nothing unexpected.
  | ExtraEvent Event -- ^ Unexpected extra event found.
  | UncheckedEvent Event -- ^ Event not found in MangoPay (cf. 'checkEvent').
  | UnhandledNotification String -- ^ Notification we couldn't parse (cf. 'eventFromQueryString').
  deriving (Show, Eq, Ord, Typeable)


-- | Wait until we receive all the expected events, and only the
-- expected events, for a maximum number of seconds.
waitForEvent
  :: ReceivedEvents
  -> [(Maybe Text, EventType)] -- ^ (Resource id, event type) expected.
  -> Integer                   -- ^ Timeout in seconds.
  -> IO EventResult
waitForEvent _ fs del | del <= 0 = return (Timeout fs)
waitForEvent rc fs del = do
  mevt <- popReceivedEvent rc
  case (mevt, fs) of
    (Nothing, []) -> return EventsOK -- Nothing left to process.
    (Just (Left er), _) -> return er -- some notification we didn't understand
    (Just (Right evt), []) -> return $ ExtraEvent evt -- an event that doesn't match
    (Nothing, _) -> do -- no event yet
      threadDelay 1000000
      waitForEvent rc fs (del-1)
    (Just (Right evt), _) -> do -- an event, does it match
      ok <- testMP $ checkEvent evt
      if ok
        then case findMatch evt fs of
          Nothing  -> return $ ExtraEvent evt -- doesn't match
          Just fs2 -> waitForEvent rc fs2 del -- matched, either we have more to do or we need to check no unexpected event was found
       else return $ UncheckedEvent evt
  where
    findMatch evt = go id
      where
        go unmatched (g@(r,t):gs)
          | testEvent r t evt = Just (unmatched gs)
          | otherwise         = go (unmatched . (g:)) gs
        go _ [] = Nothing


-- | Get one received event (and remove it from the underlying
-- storage).
popReceivedEvent :: ReceivedEvents -> IO (Maybe (Either EventResult Event))
popReceivedEvent (ReceivedEvents mv) = do
  evts <- takeMVar mv
  case evts of
    [] -> do
      putMVar mv []
      return Nothing
    (e:es) -> do
      putMVar mv es
      return $ Just e


-- | Get all received events (and remove them from the underlying
-- storage).  Useful after running all tests to ensure that all
-- events were processed.
popReceivedEvents :: ReceivedEvents -> IO [Either EventResult Event]
popReceivedEvents (ReceivedEvents mv) = do
    evts <- takeMVar mv
    putMVar mv []
    return evts


-- | Wait for the given number of seconds and check if any
-- MangoPay event has arrived.  Fails if so.  Use this function
-- at the end of your test run to ensure that you're aware of
-- every MangoPay event.
ensureNoEvents :: Int -> Assertion
ensureNoEvents seconds = do
  -- Wait in case events still arrive.
  threadDelay $ seconds * 1000000
  res  <- tsReceivedEvents <$> I.readIORef testState
  evts <- popReceivedEvents res
  assertEqual "ensureNoEvents" [] evts


----------------------------------------------------------------------


-- | The test state as a top level global variable
-- (<http://www.haskell.org/haskellwiki/Top_level_mutable_state>)
-- since HTF does not let us thread a parameter through tests.
testState :: I.IORef TestState
testState = unsafePerformIO (I.newIORef $ TestState zero zero zero zero zero)
  where zero = error "testState has not been initialized yet"
{-# NOINLINE testState #-}


-- | The test state we keep over all tests.
data TestState =
  TestState
    { tsAccessToken    :: AccessToken    -- ^ The access token if we're logged in.
    , tsCredentials    :: Credentials    -- ^ The credentials.
    , tsManager        :: H.Manager      -- ^ The HTTP manager.
    , tsHookEndPoint   :: HookEndPoint   -- ^ The end point for notifications.
    , tsReceivedEvents :: ReceivedEvents -- ^ The received events.
    }


-- | Get the HTTP manager used by the tests.
getTestHttpManager :: IO H.Manager
getTestHttpManager = tsManager <$> I.readIORef testState


-- | (Internal) The events received via the notification hook.
-- Uses an @MVar@ for storing events.
newtype ReceivedEvents = ReceivedEvents (MVar [Either EventResult Event])


-- | (Internal) Create a new 'ReceivedEvents'.
newReceivedEvents :: IO ReceivedEvents
newReceivedEvents = ReceivedEvents <$> newMVar []


----------------------------------------------------------------------


-- | Creates and initializes the MangoPay infrastructure needed
-- to use the test helpers from this module.  Tears down the
-- server at the end.
--
-- You may want to use 'ensureNoEvents' at the end of your action
-- before tearing down the server.
--
-- This module uses global, shared state.  You have to wrap your
-- entire test suite with this function, and you can't use
-- 'withMangoPayTests' more than once.
--
-- Reads two files from the current directory:
--
--  * @client.test.conf@ (can be generated by mangopay-passphrase).
--
--  * @hook.test.conf@, json file containing object with "Url"
--  and "Port" fields.  The first is the URL used to access this
--  server's root, while the second is the port this server
--  should listen to.
withMangoPayTestUtils
  :: (   Credentials
      -> AccessToken
      -> H.Manager
      -> IO a )
     -- ^ Action to run while being able to use functions from
     -- this module.
  -> IO a
withMangoPayTestUtils act =
  withWorkaroundManager $ \mgr -> liftIO $ do
    hook <- getHookEndPoint
    res  <- newReceivedEvents
    -- Initial state.
    I.modifyIORef testState $ \ts ->
      ts { tsManager        = mgr
         , tsHookEndPoint   = hook
         , tsReceivedEvents = res }
    bracket
      (startHTTPServer hook res)
      killThread
      (const $ initializeTestState >>= \(creds, at) -> act creds at mgr)


-- | Same as 'H.withManager', but without TLS 1.2 support.
--
-- As of 2014-11-22, there's a bug on the @tls@ package that
-- prevents connections to be made to the MangoPay servers.  For
-- some reason, disabling TLS 1.2 works around this problem.
-- Cf. <https://github.com/vincenthz/hs-tls/issues/87>.
--
-- This function isn't named @withTLS11Manager@ because it will
-- eventually be changed to be the same as 'H.withManager' as
-- soon as this bug is fixed.
withWorkaroundManager :: (MonadIO m, MonadBaseControl IO m) => (H.Manager -> ResourceT m a) -> m a
withWorkaroundManager act = workaroundSettings >>= flip H.withManagerSettings act


-- | Create a new 'H.Manager' with the same workarounds as
-- 'withWorkaroundManager', but does not automatically release
-- it so that you have full control of its life cycle.
newWorkaroundManager :: MonadIO m => m H.Manager
newWorkaroundManager = liftIO . H.newManager =<< workaroundSettings


-- | Settings needed for 'withWorkaroundManager'.
workaroundSettings :: MonadIO m => m H.ManagerSettings
workaroundSettings = mkSettings `liftM` liftIO ioActions
  where
    ioActions =
      X509.getSystemCertificateStore
    mkParams certStore =
      (TLS.defaultParamsClient neverHere neverHere)
        { TLS.clientSupported = def
            { TLS.supportedVersions = noTLS12
            , TLS.supportedCiphers = TLSX.ciphersuite_all }
        , TLS.clientShared = def
            { TLS.sharedCAStore = certStore }
        }
      where
        noTLS12 = filter (/= TLS.TLS12) (TLS.supportedVersions def)
        neverHere :: a
        neverHere = error "withWorkaroundManager: never here, TLSSettings/ClientParams"
    mkSettings =
      flip H.mkManagerSettings Nothing . Conn.TLSSettings . mkParams


----------------------------------------------------------------------


-- | (Internal) Read end point information from @hook.test.conf@
-- in current folder.
getHookEndPoint :: IO HookEndPoint
getHookEndPoint = do
  js <- BSL.readFile "hook.test.conf"
  let Just mhook = A.decode js
  return mhook


-- | (Internal) Simple configuration to tell the tests what
-- endpoint to use for notifications.
data HookEndPoint =
  HookEndPoint
    { hepUrl  :: Text
    , hepPort :: Int
    } deriving (Show, Read, Eq, Ord, Typeable)

instance A.ToJSON HookEndPoint where
  toJSON h =
    A.object
      [ "Url" A..= hepUrl h
      , "Port" A..= hepPort h ]

instance A.FromJSON HookEndPoint where
  parseJSON (A.Object v) =
    HookEndPoint
      <$> v A..: "Url"
      <*> v A..: "Port"
  parseJSON _ = fail "HookEndPoint"



----------------------------------------------------------------------


-- | Start a HTTP server listening on the given port.  If the
-- path info is "mphook", then we'll push the received event to
-- the given 'ReceivedEvents'.
startHTTPServer :: HookEndPoint -> ReceivedEvents -> IO ThreadId
startHTTPServer hook revts = forkIO $ run (hepPort hook) app
  where
    app req respond = runStdoutLoggingT (checkAndPushEvent req) >> respond noop
    checkAndPushEvent req
      | dropWhile T.null (W.pathInfo req) == ["mphook"] = do
          liftIO $ pushReceivedEvent revts toPush
          $(logDebugS) src (T.pack toLog)
      | otherwise =
          $(logWarnS) src $ T.pack $ concat
            [ "Received HTTP request for path "
            , show (W.pathInfo req)
            , ", ignoring." ]
      where
        (toPush, toLog) =
          case eventFromQueryString qs of
            Just evt -> (Right evt, "Received event:" ++ show evt)
            Nothing  -> ( Left $ UnhandledNotification $ show qs
                        , "Could not parse event " ++ show qs )
          where qs = W.queryString req
    noop =
      W.responseBuilder status200 [("Content-Type", "text/plain")] $
      copyByteString "noop"
    src = "mangopay-testutils"



-- | (Internal) Add a new event.
pushReceivedEvent :: ReceivedEvents -> Either EventResult Event -> IO ()
pushReceivedEvent (ReceivedEvents mv) evt = do
    evts' <- takeMVar mv
    -- we're getting events in duplicate ???
    let ns = if evt `Prelude.elem` evts' then evts' else evt:evts'
    putMVar mv ns


----------------------------------------------------------------------


-- | Initialize credentials, access token and hooks.
--
-- Takes the name/email from @client.test.conf@ in the current directory
-- (this file can be generated by mangopay-passphrase),
-- generates a new id/name, keeps the same email and generates a new secret
-- saves the secret to use in other tests.
initializeTestState :: IO (Credentials, AccessToken)
initializeTestState = do
  mgr <- getTestHttpManager
  creds <- createCredentials mgr
  at <- toAccessToken <$> createAccessToken mgr creds
  I.modifyIORef testState $ \ts ->
    ts { tsCredentials = creds
       , tsAccessToken = at }
  listenForAll
  return (creds, at)


-- | (Internal) Add the current time as a suffix to the client id
-- and name of the credentials.
addCredsSuffix :: Credentials -> POSIXTime -> Credentials
addCredsSuffix creds ct =
  creds
    { cClientSecret = Nothing
    , cClientId     = T.append (cClientId creds) suff
    , cName         = T.append (cName creds) suff }
  where suff =
          T.pack $
          reverse $ take (20 - clidlen) $ reverse $
          show (round ct :: Integer)
        clidlen = T.length $ cClientId creds


-- | (Internal) Create new 'Credentials' using the ones at
-- @client.test.conf@ and 'addCredsSuffix'.
createCredentials :: H.Manager -> IO Credentials
createCredentials mgr = do
  Just origCreds <- A.decode <$> BSL.readFile "client.test.conf"
  suffixedCreds  <- addCredsSuffix origCreds <$> getPOSIXTime
  createdCreds   <-
    runResourceT $
    runStdoutLoggingT $
    runMangoPayT suffixedCreds mgr Sandbox createCredentialsSecret
  assertBool "createCredentials/has secret" (isJust $ cClientSecret createdCreds)
  return createdCreds


-- | (Internal) Create access token from the 'Credentials'.
createAccessToken :: H.Manager -> Credentials -> IO OAuthToken
createAccessToken mgr creds =
  let Just secret = cClientSecret creds
  in runResourceT $
     runStdoutLoggingT $
     runMangoPayT creds mgr Sandbox $
     oauthLogin (cClientId creds) secret


-- | (Internal) Listen for all event types.
listenForAll :: IO ()
listenForAll = void $ AS.mapConcurrently listenFor [minBound .. maxBound]


-- | (Internal) Create a hook for a given event type.
listenFor :: EventType -> IO ()
listenFor evtT = do
  hook <- liftM tsHookEndPoint $ I.readIORef testState
  h    <- testMP $ createHook (Hook Nothing Nothing Nothing (hepUrl hook <> "/mphook") Enabled Nothing evtT)
  h2   <- testMP $ let Just id_ = hId h in fetchHook id_
  assertEqual "listenFor/id" (hId h) (hId h2)
  assertEqual "listenFor/valid" (Just Valid) (hValidity h)


----------------------------------------------------------------------


-- | Perform the full registration of a card.
--
-- This function is UNSAFE, because if you use this, YOU handle
-- the user's credit card details so you need to be PCI
-- compliant!
unsafeFullRegistration
  :: MPUsableMonad m => AnyUserId -> Currency -> CardInfo -> AccessToken -> MangoPayT m CardRegistration
unsafeFullRegistration uid currency cardInfo at = do
  -- Create registration.
  let cr1 = mkCardRegistration uid currency
  cr2 <- createCardRegistration cr1 at
  -- Register it.
  cr3 <- liftIO $ unsafeRegisterCard cardInfo cr2
  -- Save registered version.
  modifyCardRegistration cr3 at


-- | Register a card with the registration URL.
--
-- This function is UNSAFE, because if you use this, YOU handle
-- the user's credit card details so you need to be PCI
-- compliant!
--
-- XXX This is not working anymore.
unsafeRegisterCard :: CardInfo -> CardRegistration -> IO CardRegistration

unsafeRegisterCard ci cr@(CardRegistration
                            { crCardRegistrationURL = Just url
                            , crPreregistrationData = Just pre
                            , crAccessKey           = Just ak }) = do
  mgr <- getTestHttpManager
  req <- H.parseUrl $ T.unpack url
  let b =
        HT.renderQuery False $ HT.toQuery
          [ "accessKeyRef"       ?+ ak
          , "data"               ?+ pre
          , "cardNumber"         ?+ ciNumber ci
          , "cardExpirationDate" ?+ (writeCardExpiration $ ciExpire ci)
          , "cardCvx"            ?+ ciCSC ci ]
      req' =
        req
          { H.method         = HT.methodPost
          , H.requestHeaders = [("content-type", "application/x-www-form-urlencoded")]
          , H.requestBody    = H.RequestBodyBS b }
  reg <- runResourceT $ do
    res <- H.http req' mgr
    H.responseBody res $$+- EL.consume
  let t = TE.decodeUtf8 $ BS.concat reg
  assertBool
    ("unsafeRegisterCard/\"data=\" prefix of " ++ show t)
    ("data=" `T.isPrefixOf` t)
  return cr { crRegistrationData = Just t }
unsafeRegisterCard _ _ = do
  assertFailure "CardRegistration not ready"
  error "never here"
