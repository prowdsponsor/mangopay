{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.MangoPay

import Control.Monad.Logger
import Data.Aeson
import Data.Text
import Network.HTTP.Conduit
 
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)


-- | get a new secret from the given client id and name
-- shows the secret on standard out
-- write the credentials in a JSON file called client.conf in current directory
main :: IO()
main = do
        args<-getArgs
        case args of
                [cid,name,email]->do
                       let cred=Credentials (pack cid) (pack name) (pack email) Nothing
                       cred2<-withManager (\mgr->
                                runStdoutLoggingT $ runMangoPayT cred mgr Sandbox createCredentialsSecret)
                       putStrLn $ unpack $ fromJust $ cClientSecret cred2
                       BS.writeFile "client.conf" $ encode cred2
                       return ()
                _ -> putStrLn "Usage: mangopay-passphrase clientid name email"