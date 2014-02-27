{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Mangopay

import Data.Aeson
import Data.Text
import Network.HTTP.Conduit
 
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS

-- | get a new secret from the given client id and name
-- shows the secret on standard out
-- write the credentials in a JSON file called client.conf in current directory
main :: IO()
main = do
        args<-getArgs
        case args of
                [cid,name,email]->do
                       let cred=Credentials (pack cid) (pack name) (pack email) ""
                       cred2<-withManager (\mgr->
                                runMangopayT cred mgr Sandbox getPassphrase)
                       putStrLn $ unpack $ cClientSecret cred2
                       BS.writeFile "client.conf" $ encode cred2
                       return ()
                _ -> putStrLn "Usage: mangopay clientid name email"