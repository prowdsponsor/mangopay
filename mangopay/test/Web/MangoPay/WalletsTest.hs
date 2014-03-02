{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test wallets and transfers
module Web.MangoPay.WalletsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (isJust, fromJust)
import Data.Default (def)
import Control.Concurrent (killThread)
import Control.Exception (bracket)

-- | test wallet API
test_Wallet :: Assertion
test_Wallet = do
        us<-testMP $ listUsers (Just $ Pagination 1 1)
        assertEqual 1 (length us)
        let uid=urId $ head us
        let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing 
        w2<-testMP $ storeWallet w
        assertBool (isJust $ wId w2)
        assertBool (isJust $ wCreationDate w2)
        w3<-testMP $ storeWallet w2{wTag=Just "custom2"}
        assertEqual (Just "custom2") (wTag w3)
        assertEqual (wId w2) (wId w3)
        w4<-testMP $ fetchWallet (fromJust $ wId w2)
        assertEqual (Just "custom2") (wTag w4)
        assertEqual (wId w2) (wId w4)
        assertEqual (Just $ Amount "EUR" 0) (wBalance w4)
        ws<-testMP $ listWallets uid (Just $ Pagination 1 100)
        assertBool (not (null ws))
        assertEqual 1 (length $ filter ((wId w3 ==) . wId) ws)

-- | test transfer API + notifications        
test_Transfer :: Assertion
test_Transfer = do
        us<-testMP $ listUsers (Just $ Pagination 1 2)
        assertEqual 2 (length us)
        let [uid1,uid2] = map urId us
        assertBool (uid1 /= uid2)
        let w1=Wallet Nothing Nothing (Just "custom") [uid1] "my wallet" "EUR" Nothing 
        w1'<-testMP $ storeWallet w1
        let uw1=fromJust $ wId w1'
        let w2=Wallet Nothing Nothing (Just "custom") [uid2] "my wallet" "EUR" Nothing 
        w2'<-testMP $ storeWallet w2
        let uw2=fromJust $ wId w2'
        assertBool (uw1 /= uw2)
        
        hook<-getHookEndPoint
        res<-newReceivedEvents
        -- test notifications
        ok<-bracket 
          (startHTTPServer (hepPort hook) res)
          killThread
          (\_->do
                h<-testMP $ storeHook (Hook Nothing Nothing Nothing (hepUrl hook) Enabled Nothing TRANSFER_NORMAL_FAILED)
                assertBool (isJust $ hId h)
                h2<-testMP $ fetchHook (fromJust $ hId h)
                assertEqual (hId h) (hId h2)
                assertEqual (Just Valid) (hValidity h2)
                
                let t1=Transfer Nothing Nothing Nothing uid1 (Just uid2) (Amount "EUR" 100) (Amount "EUR" 1)
                        uw1 uw2 Nothing Nothing Nothing Nothing Nothing
                t1'<-testMP $ createTransfer t1
                assertBool (isJust $ tId t1')
                assertEqual (Just $ Amount "EUR" 99) (tCreditedFunds t1')
                t2'<-testMP $ fetchTransfer (fromJust $ tId t1')
                assertEqual t1' t2'
                ts1 <- testMP $ listTransfers uw1 Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . tId) ts1)
                ts2 <- testMP $ listTransfers uw2 Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . tId) ts2)
                uts1 <- testMP $ listTransfersForUser uid1 Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . tId) uts1)
                -- transfer has failed since I have no money
                -- uts2 <- testMP $ listTransfersForUser uid2 Nothing
                -- assertEqual 1 (length $ filter ((tId t1'==) . tId) uts2)
                es<-testMP $ searchEvents (def{espEventType=Just TRANSFER_NORMAL_FAILED})
                assertBool (not $ null es)
                assertBool (any ((tId t1' ==) . Just . eResourceId) es)
                
                waitForEvent res ((tId t1' ==) . Just . eResourceId) 10
          )
        assertBool ok   
        return ()
        