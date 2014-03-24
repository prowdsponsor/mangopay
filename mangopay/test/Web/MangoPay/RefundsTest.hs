{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test refunds
module Web.MangoPay.RefundsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Maybe (isJust, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

-- | test a successful card pay in + full refund
test_SimpleCardRefund :: Assertion
test_SimpleCardRefund = do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  cr<-testMP $ fullRegistration uid "EUR" testCardInfo1
  assertBool (isJust $ crCardId cr)
  let cid=fromJust $ crCardId cr
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing 
  w2<-testMP $ storeWallet w
  assertBool (isJust $ wId w2)
  let wid=fromJust $ wId w2
  (Just cp)<-testEventTypes' [PAYIN_NORMAL_CREATED,PAYIN_NORMAL_SUCCEEDED] $ do
    let cp=mkCardPayin uid uid wid (Amount "EUR" 333) (Amount "EUR" 1) "http://dummy" cid
    cp2<-testMP $ storeCardPayin cp
    assertBool (isJust $ cpId cp2)
    assertEqual (Just Succeeded) (cpStatus cp2)
    w3<-testMP $ fetchWallet wid
    assertEqual (Just $ Amount "EUR" 332) (wBalance w3)
    return $ cpId cp2
  -- PAYIN_REFUND_CREATED not received ???
  testEventTypes [PAYIN_REFUND_SUCCEEDED] $ do
    let rr=RefundRequest uid Nothing Nothing
    r<-testMP $ refundPayin cp rr
    assertEqual PAYIN (rInitialTransactionType r)
    r2<-testMP $ fetchRefund (rId r)
    assertEqual cp $ rInitialTransactionId r2
    assertEqual (Amount "EUR" 333) $ rCreditedFunds r2
    return $ Just $ rId r
    
-- | test a successful card pay in + partial refund
test_AdvancedCardRefund :: Assertion
test_AdvancedCardRefund = do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  cr<-testMP $ fullRegistration uid "EUR" testCardInfo1
  assertBool (isJust $ crCardId cr)
  let cid=fromJust $ crCardId cr
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing 
  w2<-testMP $ storeWallet w
  assertBool (isJust $ wId w2)
  let wid=fromJust $ wId w2
  (Just cp)<-testEventTypes' [PAYIN_NORMAL_CREATED,PAYIN_NORMAL_SUCCEEDED] $ do
    let cp=mkCardPayin uid uid wid (Amount "EUR" 333) (Amount "EUR" 1) "http://dummy" cid
    cp2<-testMP $ storeCardPayin cp
    assertBool (isJust $ cpId cp2)
    assertEqual (Just Succeeded) (cpStatus cp2)
    w3<-testMP $ fetchWallet wid
    assertEqual (Just $ Amount "EUR" 332) (wBalance w3)
    return $ cpId cp2
  -- PAYIN_REFUND_CREATED not received ???  
  testEventTypes [PAYIN_REFUND_SUCCEEDED] $ do
    let rr=RefundRequest uid (Just $ Amount "EUR" 100) (Just $ Amount "EUR" 1) 
    r<-testMP $ refundPayin cp rr
    assertEqual PAYIN (rInitialTransactionType r)
    r2<-testMP $ fetchRefund (rId r)
    assertEqual cp $ rInitialTransactionId r2
    assertEqual (Amount "EUR" 99) $ rCreditedFunds r2
    return $ Just $ rId r    
 
-- | test transfer + full refund
test_TransferRefund :: Assertion
test_TransferRefund = do
        usL<-testMP $ listUsers (Just $ Pagination 1 2)
        assertEqual 2 (length $ plData usL)
        let [uid1,uid2] = map urId $ plData usL
        assertBool (uid1 /= uid2)
        let w1=Wallet Nothing Nothing (Just "custom") [uid1] "my wallet" "EUR" Nothing 
        w1'<-testMP $ storeWallet w1
        let uw1=fromJust $ wId w1'
        let w2=Wallet Nothing Nothing (Just "custom") [uid2] "my wallet" "EUR" Nothing 
        w2'<-testMP $ storeWallet w2
        let uw2=fromJust $ wId w2'
        assertBool (uw1 /= uw2)
        
        cr<-testMP $ fullRegistration uid1 "EUR" testCardInfo1
        assertBool (isJust $ crCardId cr)
        let cid=fromJust $ crCardId cr
        testEventTypes [PAYIN_NORMAL_CREATED,PAYIN_NORMAL_SUCCEEDED] $ do
          let cp=mkCardPayin uid1 uid1 uw1 (Amount "EUR" 333) (Amount "EUR" 1) "http://dummy" cid
          cp2<-testMP $ storeCardPayin cp
          assertEqual (Just Succeeded) (cpStatus cp2)
          return $ cpId cp2

        (Just tr)<-testEventTypes' [TRANSFER_NORMAL_CREATED,TRANSFER_NORMAL_SUCCEEDED] $ do
                let t1=Transfer Nothing Nothing Nothing uid1 (Just uid2) (Amount "EUR" 100) (Amount "EUR" 1)
                        uw1 uw2 Nothing Nothing Nothing Nothing Nothing
                t1'<-testMP $ createTransfer t1
                assertBool (isJust $ tId t1')
                assertEqual (Just $ Amount "EUR" 99) (tCreditedFunds t1')
                t2'<-testMP $ fetchTransfer (fromJust $ tId t1')
                assertEqual t1' t2'
                ts1 <- testMP $ listTransactions uw1 Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts1)
                ts2 <- testMP $ listTransactions uw2 Nothing
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData ts2)
                uts1 <- testMP $ listTransactionsForUser uid1 (Just $ Pagination 1 50)
                assertEqual 1 (length $ filter ((tId t1'==) . txId) $ plData uts1)             
                return $ tId t1' 
                
        testEventTypes [TRANSFER_REFUND_CREATED,TRANSFER_REFUND_SUCCEEDED] $ do
          r<-testMP $ refundTransfer tr uid1
          assertEqual TRANSFER (rInitialTransactionType r)
          r2<-testMP $ fetchRefund (rId r)
          assertEqual tr $ rInitialTransactionId r2
          assertEqual (Amount "EUR" 100) $ rCreditedFunds r2
          return $ Just $ rId r      