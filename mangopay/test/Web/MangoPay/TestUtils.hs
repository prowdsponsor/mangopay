{-# LANGUAGE RankNTypes,OverloadedStrings,DeriveDataTypeable #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.TestUtils where

import Web.MangoPay

import Data.ByteString.Lazy as BS hiding (map)
import Network.HTTP.Conduit as H
import Data.Conduit
import Data.Maybe
import Test.Framework

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Aeson as A
import Data.Monoid
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import Data.Typeable
import Control.Applicative

-- | file path to test client conf file
testConfFile :: FilePath
testConfFile="client.test.conf"

-- | test MangoPay API call, logging in with the client credentials
-- expects a file called client.test.conf containing the JSON of client credentials
-- in the current directory 
testMP :: forall b.
            (AccessToken -> MangoPayT (ResourceT IO) b)
            -> IO b
testMP f= do
        js<-BS.readFile testConfFile
        let mcred=decode js
        assertBool (isJust mcred)
        let cred=fromJust mcred
        assertBool (isJust $ cClientSecret cred)
        let s=fromJust $ cClientSecret cred
        H.withManager (\mgr->
          runMangoPayT cred mgr Sandbox (do
                at<-oauthLogin (cClientID cred) s
                f at
                ))

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
        events::MVar [Event]
        }

-- | creates the new ReceivedEvents
newReceivedEvents :: IO ReceivedEvents
newReceivedEvents=do
        mv<-newMVar []          
        return $ ReceivedEvents mv

-- | wait till we receive the expected event, for a maximum number of seconds
waitForEvent :: ReceivedEvents 
  -> (Event -> Bool) -- ^ function on the expected event
  -> Integer -- ^ delay in seconds
  -> IO Bool
waitForEvent _ _ del | del<=0=return False
waitForEvent rc f del=do
        evts<-popReceivedEvents rc
        if Prelude.any f evts
                then return True
                else do
                        threadDelay 1000000
                        waitForEvent rc f (del-1)

-- | get all received events (and remove them from the underlying storage)        
popReceivedEvents :: ReceivedEvents -> IO [Event]
popReceivedEvents (ReceivedEvents mv)=do
        evts<-takeMVar mv                
        putMVar mv []
        return evts

-- | add a new event
pushReceivedEvent :: ReceivedEvents -> Event -> IO ()
pushReceivedEvent (ReceivedEvents mv) evt=do
        evts' <-takeMVar mv    
        putMVar mv (evt : evts')
        return ()

-- | start a HTTP server listening on the given port
-- if the path info is "mphook", then we'll push the received event                        
startHTTPServer :: Port -> ReceivedEvents -> IO ThreadId
startHTTPServer p revts= 
  forkIO $ run p app
  where
    app req = do
                when (pathInfo req == ["mphook"]) $ do
                        liftIO $ Prelude.putStrLn "mphook start"
                        let mevt=eventFromQueryString $ W.queryString req
                        liftIO $ case mevt of
                            Just evt->do
                                pushReceivedEvent revts evt
                                Prelude.putStrLn "Received!!"
                                print evt
                            Nothing->Prelude.putStrLn "Couldn't parse Event"
                return $ ResponseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString ["noop"]
                
