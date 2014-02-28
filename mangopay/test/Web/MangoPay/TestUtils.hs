{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.TestUtils where

import Web.MangoPay

import Data.ByteString.Lazy as BS
import Data.Aeson
import Network.HTTP.Conduit
import Data.Conduit
import Data.Maybe
import Test.Framework


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
        withManager (\mgr->
          runMangoPayT cred mgr Sandbox (do
                at<-oauthLogin (cClientID cred) s
                f at
                ))