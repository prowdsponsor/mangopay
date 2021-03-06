{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test wallets and transfers
module Web.MangoPay.WalletsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Default
import Data.Maybe (isJust, fromJust)

-- | test wallet API
test_Wallet :: Assertion
test_Wallet = do
        usL<-testMP $ listUsers def (Just $ Pagination 1 1)
        assertEqual 1 (length $ plData usL)
        let uid=urId $ head $ plData usL
        let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing
        w2<-testMP $ createWallet w
        assertBool (isJust $ wId w2)
        assertBool (isJust $ wCreationDate w2)
        w3<-testMP $ modifyWallet w2{wTag=Just "custom2"}
        assertEqual (Just "custom2") (wTag w3)
        assertEqual (wId w2) (wId w3)
        w4<-testMP $ fetchWallet (fromJust $ wId w2)
        assertEqual (Just "custom2") (wTag w4)
        assertEqual (wId w2) (wId w4)
        assertEqual (Just $ Amount "EUR" 0) (wBalance w4)
        ws<-testMP $ listWallets uid def (Just $ Pagination 1 100)
        assertBool (not (null $ plData ws))
        assertEqual 1 (length $ filter ((wId w3 ==) . wId) $ plData ws)

-- | test transfer API + notifications on failure
test_FailedTransfer :: Assertion
test_FailedTransfer = do
        usL<-testMP $ listUsers def (Just $ Pagination 1 2)
        assertEqual 2 (length $ plData usL)
        let [uid1,uid2] = map urId $ plData usL
        assertBool (uid1 /= uid2)
        ws<- testMP $ listWallets uid1 def Nothing
        assertBool $ not $ null $ plData ws
        let uw1=fromJust $ wId $ head $ plData ws
        let w2=Wallet Nothing Nothing (Just "custom") [uid2] "my wallet" "EUR" Nothing
        w2'<-testMP $ createWallet w2
        let uw2=fromJust $ wId w2'
        assertBool (uw1 /= uw2)
        -- transfer will fail since I have no money
        testEventTypes [TRANSFER_NORMAL_CREATED {- Fixed in Okapi <http://docs.mangopay.com/release-okapi-hook-fixes-and-new-sort-options/> -}
                  , TRANSFER_NORMAL_FAILED] $ do
                let t1=Transfer Nothing Nothing Nothing uid1 (Just uid2) (Amount "EUR" 100) (Amount "EUR" 1)
                        uw1 uw2 Nothing Nothing Nothing Nothing Nothing
                t1'<-testMP $ createTransfer t1
                assertBool (isJust $ tId t1')
                assertEqual (Just $ Amount "EUR" 99) (tCreditedFunds t1')
                t2'<-testMP $ fetchTransfer (fromJust $ tId t1')
                assertEqual t1' t2'
                ts1 <- testMP $ listTransactions uw1 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts1)
                ts1f <- testMP $ listTransactions uw1 (def{tfStatus=Just Failed}) def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts1f)
                ts1ft <- testMP $ listTransactions uw1 (def{tfStatus=Just Failed,tfType=Just TRANSFER}) def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts1ft)
                ts1st <- testMP $ listTransactions uw1 (def{tfStatus=Just Succeeded,tfType=Just TRANSFER}) def Nothing
                assertEqual 0 (length $ filter ((tId t1'==) . txId) $ plData ts1st)
                ts2 <- testMP $ listTransactions uw2 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts2)
                uts1 <- testMP $ listTransactionsForUser uid1 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData uts1)
                return $ tId t1'

-- | test transfer API + notifications on success
test_SuccessfulTransfer :: Assertion
test_SuccessfulTransfer = do
        usL<-testMP $ listUsers def (Just $ Pagination 1 2)
        assertEqual 2 (length $ plData usL)
        let [uid1,uid2] = map urId $ plData usL
        assertBool (uid1 /= uid2)
        ws<- testMP $ listWallets uid1 def Nothing
        assertBool $ not $ null $ plData ws
        let uw1=fromJust $ wId $ head $ plData ws
        let w2=Wallet Nothing Nothing (Just "custom") [uid2] "my wallet" "EUR" Nothing
        w2'<-testMP $ createWallet w2
        let uw2=fromJust $ wId w2'
        assertBool (uw1 /= uw2)

        cr<-testMP $ unsafeFullRegistration uid1 "EUR" testCardInfo1
        assertBool (isJust $ crCardId cr)
        let cid=fromJust $ crCardId cr
        testEventTypes [PAYIN_NORMAL_CREATED,PAYIN_NORMAL_SUCCEEDED] $ do
          let cp=mkCardPayin uid1 uid1 uw1 (Amount "EUR" 333) (Amount "EUR" 1) "http://dummy" cid
          cp2<-testMP $ createCardPayin cp
          assertEqual (Just Succeeded) (cpStatus cp2)
          return $ cpId cp2

        testEventTypes [TRANSFER_NORMAL_CREATED {- Fixed in Okapi <http://docs.mangopay.com/release-okapi-hook-fixes-and-new-sort-options/> -}
                  , TRANSFER_NORMAL_SUCCEEDED] $ do
                let t1=Transfer Nothing Nothing Nothing uid1 (Just uid2) (Amount "EUR" 100) (Amount "EUR" 1)
                        uw1 uw2 Nothing Nothing Nothing Nothing Nothing
                t1'<-testMP $ createTransfer t1
                assertBool (isJust $ tId t1')
                assertEqual (Just $ Amount "EUR" 99) (tCreditedFunds t1')
                t2'<-testMP $ fetchTransfer (fromJust $ tId t1')
                assertEqual t1' t2'
                ts1 <- testMP $ listTransactions uw1 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts1)
                ts2 <- testMP $ listTransactions uw2 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts2)
                uts1 <- testMP $ listTransactionsForUser uid1 def def Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData uts1)
                return $ tId t1'


test_OrderTransactions :: Assertion
test_OrderTransactions = do
  usL<-testMP $ listUsers def (Just $ Pagination 1 2)
  assertEqual 2 (length $ plData usL)
  let uid1 = head $ map urId $ plData usL
  uts1 <- testMP $ listTransactionsForUser uid1 def (TxByExecutionDate ASC) Nothing
  assertBool $ length (plData uts1) > 1
  uts2 <- testMP $ listTransactionsForUser uid1 def (TxByExecutionDate DESC) Nothing
  assertEqual (map txId $ plData uts1) $ reverse $ map txId $ plData uts2
  utsc1 <- testMP $ listTransactionsForUser uid1 def (TxByCreationDate ASC) Nothing
  assertBool $ length (plData utsc1) > 1
  utsc2 <- testMP $ listTransactionsForUser uid1 def (TxByCreationDate DESC) Nothing
  assertEqual (map txId $ plData utsc1) $ reverse $ map txId $ plData utsc2

test_OrderEvents :: Assertion
test_OrderEvents = do
  evt1 <- testMP $  searchAllEvents def{espSortByDate=Just ASC}
  evt2 <- testMP $  searchAllEvents def{espSortByDate=Just DESC}
  -- we just check the resource id because the failed transfer may have the same date
  -- as the creation, so we can't compare on the full even (with status)
  assertEqual (map eResourceId evt1) $ reverse (map eResourceId evt2)
