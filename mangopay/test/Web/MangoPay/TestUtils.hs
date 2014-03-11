{-# LANGUAGE RankNTypes,OverloadedStrings,DeriveDataTypeable #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.TestUtils where

import Web.MangoPay

import Data.ByteString.Lazy as BS hiding (map,any,null)
import Network.HTTP.Conduit as H
import Data.Conduit
import Data.Maybe
import Test.Framework

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Aeson as A
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when, void, liftM)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import Data.List
import Data.Typeable
import Control.Applicative
import Test.HUnit (Assertion)
import Data.Default (def)
import Data.IORef 
import System.IO.Unsafe (unsafePerformIO)

-- | a test card
testCardInfo1 :: CardInfo
testCardInfo1 = CardInfo "4970100000000154" "1220" "123"

-- | test MangoPay API call, logging in with the client credentials
-- expects a file called client.test.conf containing the JSON of client credentials
-- in the current directory 
testMP :: forall b.
            (AccessToken -> MangoPayT (ResourceT IO) b)
            -> IO b
testMP f= do
        ior<-readIORef testState  
        let mgr=tsManager ior 
        let at=tsAccessToken ior
        let cred=tsCredentials ior
        runResourceT $ runMangoPayT cred mgr Sandbox $ f at

-- | the test state as a top level global variable
-- <http://www.haskell.org/haskellwiki/Top_level_mutable_state>
-- since HTF does not let us thread a parameter through tests
testState :: IORef TestState
{-# NOINLINE testState #-}
testState = unsafePerformIO (newIORef $ TestState zero zero zero zero zero)
  where zero=error "testState has not been initialized yet"


-- | the test state we keep over all tests
data TestState=TestState {
    tsAccessToken :: AccessToken -- ^ the access token if we're logged in
    ,tsCredentials :: Credentials -- ^ the credentials
    ,tsManager :: H.Manager -- ^ the http manager
    ,tsHookEndPoint :: HookEndPoint -- ^ the end point for notifications
    ,tsReceivedEvents :: ReceivedEvents -- ^ the received events
  }
  

-- | read end point information from hook.test.conf in current folder
getHookEndPoint :: IO HookEndPoint
getHookEndPoint = do
      js<-BS.readFile "hook.test.conf"
      let mhook=decode js  
      assertBool (isJust mhook)
      return $ fromJust mhook

-- | simple configuration to tell the tests what endpoint to use for notifications
data HookEndPoint = HookEndPoint{
        hepUrl :: Text
        ,hepPort :: Int
        } deriving (Show,Read,Eq,Ord,Typeable)

-- | to json        
instance ToJSON HookEndPoint where
        toJSON h=object ["Url"  .= hepUrl h,"Port" .= hepPort h]

-- | from json 
instance FromJSON HookEndPoint where
        parseJSON (Object v) =HookEndPoint <$>
                         v .: "Url" <*>
                         v .: "Port" 
        parseJSON _=fail "HookEndPoint"


-- | the events received via the notification hook
-- uses a MVar for storing events
data ReceivedEvents=ReceivedEvents{
        events::MVar [Either EventResult Event]
        }

-- | creates the new ReceivedEvents
newReceivedEvents :: IO ReceivedEvents
newReceivedEvents=do
        mv<-newMVar []          
        return $ ReceivedEvents mv

-- | test an event, checking the resource id and the event type
testEvent :: Maybe Text -> EventType -> Event -> Bool
testEvent tid et evt= tid == (Just $ eResourceId evt) 
        && et == eEventType evt

-- | check that we're receiving events of the given type with the resource id returned by the passed test
testEventTypes :: [EventType] 
  -> IO (Maybe Text)
  -> Assertion
testEventTypes evtTs =void . testEventTypes' evtTs 

-- | check that we're receiving events of the given type with the resource id returned by the passed test
-- returns the result of the inner test
testEventTypes' :: [EventType] 
  -> IO (Maybe Text)
  -> IO (Maybe Text)
testEventTypes' evtTs ops=do
    res<-liftM tsReceivedEvents $ readIORef testState
    a<-ops
    mapM_ (testSearchEvent a) evtTs
    er<-waitForEvent res (map (testEvent a) evtTs) 5
    assertEqual EventsOK er
    return a

-- | assert that we find an event for the given resource id and event type
testSearchEvent :: Maybe Text -> EventType -> Assertion
testSearchEvent tid evtT=do
  es<-testMP $ searchEvents (def{espEventType=Just evtT})
  assertBool (not $ null es)
  assertBool (any ((tid ==) . Just . eResourceId) es)

-- | create a hook for a given event type    
createHook :: EventType -> Assertion
createHook evtT= do
    hook<-liftM tsHookEndPoint $ readIORef testState
    h<-testMP $ storeHook (Hook Nothing Nothing Nothing (hepUrl hook) Enabled Nothing evtT)
    assertBool (isJust $ hId h)
    h2<-testMP $ fetchHook (fromJust $ hId h)
    assertEqual (hId h) (hId h2)
    assertEqual (Just Valid) (hValidity h)
    
-- | run a test with the notification server running
testEvents :: IO a -- ^ the test, returning a value
  -> [a -> Event -> Bool] -- ^ the test on the events, taking into account the returned value
  -> Assertion
testEvents ops tests=do
    res<-liftM tsReceivedEvents $ readIORef testState
    a<-ops
    er<-waitForEvent res (map ($ a) tests) 30
    assertEqual EventsOK er
            
-- | result of waiting for event
data EventResult = Timeout -- ^ didn't receive all expected events
  | EventsOK -- ^ OK: everything expected received, nothing unexpected
  | ExtraEvent Event -- ^ unexpected
  | UnhandledNotification String -- ^ notification we couldn't parse
  deriving (Show,Eq,Ord,Typeable)

-- | wait till we receive all the expected events, and none other, for a maximum number of seconds
waitForEvent :: ReceivedEvents 
  -> [Event -> Bool] -- ^ function on the expected event
  -> Integer -- ^ delay in seconds
  -> IO EventResult
waitForEvent _ _ del | del<=0=return Timeout
waitForEvent rc fs del=do
        mevt<-popReceivedEvent rc
        case (mevt,fs) of
          (Nothing,[])->return EventsOK -- nothing left to process
          (Just (Left er),_)->return er -- some notification we didn't understand
          (Just (Right evt),[])->return $ ExtraEvent evt -- an event that doesn't match
          (Nothing,_)->do -- no event yet
             threadDelay 1000000
             waitForEvent rc fs (del-1)
          (Just (Right evt),_)-> -- an event, does it match
                case Data.List.foldl' (match1 evt) ([],False) fs of
                  (_,False)->return $ ExtraEvent evt -- doesn't match
                  (fs2,_)-> waitForEvent rc fs2 del -- matched, either we have more to do or we need to check no unexpected event was found
                       
  where 
    -- | match the first event function and return all the non matching function, and a flag indicating if we matched
    match1 :: Event -> ([Event -> Bool],Bool) -> (Event -> Bool) -> ([Event -> Bool],Bool)
    match1 evt (nfs,False) f
      | f evt=(nfs,True)
      | otherwise=(f:nfs,False)
    match1 _ (nfs,True) f=(f:nfs,True)

-- | get one received event (and remove it from the underlying storage)        
popReceivedEvent :: ReceivedEvents -> IO (Maybe (Either EventResult Event))
popReceivedEvent (ReceivedEvents mv)=do
        evts<-takeMVar mv                
        case evts of
          []->do
                putMVar mv []
                return Nothing
          (e:es)->do
                putMVar mv es
                return $ Just e

-- | get all received events (and remove them from the underlying storage)        
popReceivedEvents :: ReceivedEvents -> IO [Either EventResult Event]
popReceivedEvents (ReceivedEvents mv)=do
        evts<-takeMVar mv                
        putMVar mv []
        return evts

-- | add a new event
pushReceivedEvent :: ReceivedEvents -> Either EventResult Event -> IO ()
pushReceivedEvent (ReceivedEvents mv) evt=do
        evts' <-takeMVar mv   
        -- we're getting events in duplicate ???
        let ns = if evt `Prelude.elem` evts' then evts' else evt:evts'
        putMVar mv ns

-- | start a HTTP server listening on the given port
-- if the path info is "mphook", then we'll push the received event                        
startHTTPServer :: Port -> ReceivedEvents -> IO ThreadId
startHTTPServer p revts= 
  forkIO $ run p app
  where
    app req = do
                when (pathInfo req == ["mphook"]) $ do
                        let mevt=eventFromQueryString $ W.queryString req
                        liftIO $ case mevt of
                            Just evt->do
                                pushReceivedEvent revts $ Right evt
                                print evt
                            Nothing->pushReceivedEvent revts $ Left $ UnhandledNotification $ show $ W.queryString req
                return $ ResponseBuilder status200 [("Content-Type", "text/plain")] $ copyByteString "noop"
                
