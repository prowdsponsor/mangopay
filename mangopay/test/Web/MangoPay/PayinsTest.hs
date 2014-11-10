{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test payins
module Web.MangoPay.PayinsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Maybe (isJust, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

-- | test bankwire
test_BankWire :: Assertion
test_BankWire=do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing
  w2<-testMP $ createWallet w
  assertBool (isJust $ wId w2)
  -- bank wire does not succeed since the real transfer needs to be performed afterwards
  testEventTypes [PAYIN_NORMAL_CREATED] $ do
    let bw1=mkBankWire uid uid (fromJust $ wId w2) (Amount "EUR" 100) (Amount "EUR" 1)
    bw2<-testMP $ createBankWirePayIn bw1
    assertBool (isJust $ bwId bw2)
    assertBool (isJust $ bwBankAccount bw2)
    bw3<-testMP $ fetchBankWirePayIn (fromJust $ bwId bw2)
    assertEqual (bwId bw2) (bwId bw3)
    return $ bwId bw2

-- | test a successful card pay in
test_CardOK :: Assertion
test_CardOK = do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  cr<-testMP $ unsafeFullRegistration uid "EUR" testCardInfo1
  assertBool (isJust $ crCardId cr)
  let cid=fromJust $ crCardId cr
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing
  w2<-testMP $ createWallet w
  assertBool (isJust $ wId w2)
  let wid=fromJust $ wId w2
  testEventTypes [PAYIN_NORMAL_CREATED,PAYIN_NORMAL_SUCCEEDED] $ do
    let cp=mkCardPayin uid uid wid (Amount "EUR" 333) (Amount "EUR" 1) "http://dummy" cid
    cp2<-testMP $ createCardPayin cp
    assertBool (isJust $ cpId cp2)
    assertEqual (Just Succeeded) (cpStatus cp2)
    w3<-testMP $ fetchWallet wid
    assertEqual (Just $ Amount "EUR" 332) (wBalance w3)
    ts1 <- testMP $ listTransactions wid Nothing
    assertEqual 1 (length $ filter ((cpId cp2==) . txId) $ plData ts1)
    return $ cpId cp2

-- | test a failed card pay in
-- according to <http://docs.mangopay.com/api-references/test-payment/>
test_CardKO :: Assertion
test_CardKO = do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  cr<-testMP $ unsafeFullRegistration uid "EUR" testCardInfo1
  assertBool (isJust $ crCardId cr)
  let cid=fromJust $ crCardId cr
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing
  w2<-testMP $ createWallet w
  assertBool (isJust $ wId w2)
  let wid=fromJust $ wId w2
  testEventTypes [PAYIN_NORMAL_CREATED {- ,PAYIN_NORMAL_FAILED not thrown?? -}] $ do
    let cp=mkCardPayin uid uid wid (Amount "EUR" 33394) (Amount "EUR" 0) "http://dummy" cid
    cp2<-testMP $ createCardPayin cp
    assertBool (isJust $ cpId cp2)
    assertEqual (Just Failed) (cpStatus cp2)
    assertEqual (Just "009199") (cpResultCode cp2)
    assertEqual (Just "PSP technical error") (cpResultMessage cp2)
    w3<-testMP $ fetchWallet wid
    assertEqual (Just $ Amount "EUR" 0) (wBalance w3)
    return $ cpId cp2

