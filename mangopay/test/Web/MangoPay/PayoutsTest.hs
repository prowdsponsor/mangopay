{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test payouts
module Web.MangoPay.PayoutsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Default
import Data.Maybe (isJust, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

-- | test successful payout
test_PayoutOK :: Assertion
test_PayoutOK=do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  accs<-testMP $ listAccounts uid def Nothing
  assertBool $ not $ null $ plData accs
  let aid=fromJust $ baId $ head $ plData accs
  ws<- testMP $ listWallets uid def Nothing
  assertBool $ not $ null $ plData ws
  let wid=fromJust $ wId $ head $ plData ws
  let Just (Amount _ nb) =wBalance $ head $ plData ws
  assertBool $ nb >= 100
  -- Fixed in Okapi <http://docs.mangopay.com/release-okapi-hook-fixes-and-new-sort-options/>
  -- IMPORTANT: we don't get the PAYOUT_NORMAL_SUCCESSFUL since the payout needs to be validated
  testEventTypes [PAYOUT_NORMAL_CREATED] $ do
    let pt1=(mkPayout uid wid (Amount "EUR" 100) (Amount "EUR" 0) aid){ptBankWireRef=Just "ref"}
    pt2<-testMP $ createPayout pt1
    assertBool $ isJust $ ptId pt2
    assertEqual (Just Created) (ptStatus pt2)
    assertEqual (Just (Amount "EUR" 100)) (ptCreditedFunds pt2)
    pt3 <- testMP $ fetchPayout $ fromJust $  ptId pt2
    assertEqual (Just BANK_WIRE) (ptPaymentType pt3)
    assertEqual (Just "ref") (ptBankWireRef pt3)
    return $ ptId pt2

-- | test failing payout
test_PayoutKO :: Assertion
test_PayoutKO=do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  accs<-testMP $ listAccounts uid def Nothing
  assertBool $ not $ null $ plData accs
  let aid=fromJust $ baId $ head $ plData accs
  ws<- testMP $ listWallets uid def Nothing
  assertBool $ not $ null $ plData ws
  let wid=fromJust $ wId $ head $ plData ws
  -- Fixed in Okapi <http://docs.mangopay.com/release-okapi-hook-fixes-and-new-sort-options/>
  testEventTypes [PAYOUT_NORMAL_CREATED,PAYOUT_NORMAL_FAILED] $ do
    let pt1=mkPayout uid wid (Amount "EUR" 100000) (Amount "EUR" 0) aid
    pt2<-testMP $ createPayout pt1
    assertBool $ isJust $ ptId pt2
    assertEqual (Just Failed) (ptStatus pt2)
    pt3 <- testMP $ fetchPayout $ fromJust $  ptId pt2
    assertEqual (Just BANK_WIRE) (ptPaymentType pt3)
    assertEqual (Just "001001") (ptResultCode pt3)
    assertEqual (Just "Unsufficient wallet balance") (ptResultMessage pt3)
    return $ ptId pt2


