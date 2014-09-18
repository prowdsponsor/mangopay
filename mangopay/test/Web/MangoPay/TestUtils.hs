{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             OverloadedStrings, PatternGuards, RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.TestUtils
  ( testCardInfo1
  , testMP
  , testState -- TODO: This shouldn't be exported!
  , TestState(..) -- TODO: This shouldn't be exported!
  , getTestHttpManager
  -- , testEvent
  , testEventTypes
  , testEventTypes'
  -- , testSearchEvent
  , listenFor -- TODO: This shouldn't be exported!
  -- , testEvents
  -- , waitForEvent
  -- , match1
  -- , popReceivedEvent
  , popReceivedEvents
  -- , pushReceivedEvent
  , unsafeFullRegistration
  , unsafeRegisterCard

  , newReceivedEvents -- TODO: This shouldn't be exported!
  , HookEndPoint(..) -- TODO: This shouldn't be exported!
  , getHookEndPoint -- TODO: This shouldn't be exported!
  , startHTTPServer

  ) where

import Blaze.ByteString.Builder (copyByteString)
import Control.Applicative
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when, void, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit (($$+-))
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework
import Test.HUnit (Assertion)

import Web.MangoPay

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as H
import qualified Data.Aeson as A
import qualified Data.Conduit.List as EL
import qualified Data.IORef as I
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as W


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


-- | Read end point information from @hook.test.conf@ in current
-- folder.
getHookEndPoint :: IO HookEndPoint
getHookEndPoint = do
  js <- BSL.readFile "hook.test.conf"
  let Just mhook = A.decode js
  return mhook


-- | Get the HTTP manager used by the tests.
getTestHttpManager :: IO H.Manager
getTestHttpManager = tsManager <$> I.readIORef testState


-- | Simple configuration to tell the tests what endpoint to use
-- for notifications.
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


-- | The events received via the notification hook.
-- Uses an @MVar@ for storing events.
newtype ReceivedEvents = ReceivedEvents (MVar [Either EventResult Event])


-- | Create a new 'ReceivedEvents'.
newReceivedEvents :: IO ReceivedEvents
newReceivedEvents = ReceivedEvents <$> newMVar []


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


-- | Same as 'testEventTypes', but also return the resource id.
testEventTypes' :: [EventType] -> IO (Maybe Text) -> IO (Maybe Text)
testEventTypes' evtTs ops = do
  res <- liftM tsReceivedEvents $ I.readIORef testState
  a <- ops
  mapM_ (testSearchEvent a) evtTs
  er <- waitForEvent res (map (testEvent a) evtTs) 5
  assertEqual EventsOK er
  return a


-- | Assert that we find an event for the given resource id and
-- event type.
testSearchEvent :: Maybe Text -> EventType -> Assertion
testSearchEvent tid evtT = do
  es <- testMP $ searchAllEvents (def { espEventType = Just evtT })
  assertBool (not $ null es)
  assertBool (any ((tid ==) . Just . eResourceId) es)


-- | Create a hook for a given event type.
listenFor :: EventType -> Assertion
listenFor evtT = do
  hook <- liftM tsHookEndPoint $ I.readIORef testState
  h <- testMP $ createHook (Hook Nothing Nothing Nothing (hepUrl hook <> "/mphook") Enabled Nothing evtT)
  h2 <- testMP $ let Just id_ = hId h in fetchHook id_
  assertEqual (hId h) (hId h2)
  assertEqual (Just Valid) (hValidity h)


-- | Run a test with the notification server running.
testEvents :: IO a -- ^ The test, returning a value
  -> [a -> Event -> Bool] -- ^ The test on the events, taking into account the returned value
  -> Assertion
testEvents ops tests = do
  res <- liftM tsReceivedEvents $ I.readIORef testState
  a <- ops
  er <- waitForEvent res (map ($ a) tests) 5
  assertEqual EventsOK er


-- | Result of waiting for an event.
data EventResult =
    Timeout -- ^ Didn't receive all expected events within the timeout.
  | EventsOK -- ^ OK: everything expected received, nothing unexpected.
  | ExtraEvent Event -- ^ Unexpected extra event found.
  | UncheckedEvent Event -- ^ Event not found in MangoPay (cf. 'checkEvent').
  | UnhandledNotification String -- ^ Notification we couldn't parse (cf. 'eventFromQueryString').
  deriving (Show, Eq, Ord, Typeable)


-- | Wait until we receive all the expected events, and only the
-- expected events, for a maximum number of seconds.
waitForEvent
  :: ReceivedEvents
  -> [Event -> Bool] -- ^ Function on the expected event.
  -> Integer         -- ^ Timeout in seconds.
  -> IO EventResult
waitForEvent _ _ del | del <= 0 = return Timeout
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
        go unmatched (g:gs)
          | g evt     = Just (unmatched gs)
          | otherwise = go (unmatched . (g:)) gs
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


-- | (Internal) Add a new event.
pushReceivedEvent :: ReceivedEvents -> Either EventResult Event -> IO ()
pushReceivedEvent (ReceivedEvents mv) evt = do
    evts' <- takeMVar mv
    -- we're getting events in duplicate ???
    let ns = if evt `Prelude.elem` evts' then evts' else evt:evts'
    putMVar mv ns


-- | Start a HTTP server listening on the given port.  If the
-- path info is "mphook", then we'll push the received event to
-- the given 'ReceivedEvents'.
startHTTPServer :: Port -> ReceivedEvents -> IO ThreadId
startHTTPServer p revts = forkIO $ run p app
  where
    app req respond = do
      when (W.pathInfo req == ["mphook"]) $ do
        let mevt = eventFromQueryString $ W.queryString req
        liftIO $ case mevt of
          Just evt -> do
            pushReceivedEvent revts $ Right evt
            putStrLn $ "Received event:" ++ show evt
          Nothing -> pushReceivedEvent revts $ Left $ UnhandledNotification $ show $ W.queryString req
      respond $ W.responseBuilder status200 [("Content-Type", "text/plain")] $ copyByteString "noop"


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
unsafeRegisterCard :: CardInfo -> CardRegistration -> IO CardRegistration

unsafeRegisterCard ci cr@(CardRegistration
                            { crCardRegistrationURL = Just url
                            , crPreregistrationData = Just pre
                            , crAccessKey           = Just ak }) = do
  ior <- I.readIORef testState
  let mgr = tsManager ior
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
  assertBool $ "data = " `T.isPrefixOf` t
  return cr { crRegistrationData = Just t }
unsafeRegisterCard _ _ = assertFailure "CardRegistration not ready"
