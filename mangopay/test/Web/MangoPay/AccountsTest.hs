{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test accounts
module Web.MangoPay.AccountsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Maybe (isJust, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

-- | test bank account creation
test_BankAccount :: Assertion
test_BankAccount=do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  let details=IBAN "FR3020041010124530725S03383" "CRLYFRPP"
  let acc1=BankAccount Nothing Nothing (Just uid) Nothing details "JP Moresmau" (Just "Earth")
  acc2<-testMP $ createAccount acc1
  assertBool $ isJust $ baId acc2
  assertBool $ isJust $ baCreationDate acc2
  assertEqual details $ baDetails acc2
  acc3<-testMP $ fetchAccount uid $ fromJust $ baId acc2
  assertEqual details $ baDetails acc3
  accs<-testMP $ listAccounts uid Nothing
  assertBool $ acc3  `elem` (plData accs)

