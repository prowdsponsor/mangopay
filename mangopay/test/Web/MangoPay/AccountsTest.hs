{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test accounts
module Web.MangoPay.AccountsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Default
import Control.Monad (guard)
import Data.Maybe (isJust, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

-- | Test bank account creation using IBAN with BIC.
test_BankAccount_IBAN_BIC :: Assertion
test_BankAccount_IBAN_BIC = doBankAccountTest True


-- | Test bank account creation using IBAN without BIC.
test_BankAccount_IBAN_only :: Assertion
test_BankAccount_IBAN_only = doBankAccountTest False


-- | Do the bank account test, using the BIC or not.
doBankAccountTest :: Bool -> Assertion
doBankAccountTest useBIC = do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid  = urId $ head $ plData usL
      iban = "FR3020041010124530725S03383"
      bic  = "CRLYFRPP"
      details = IBAN iban (guard useBIC >> return bic)
      acc1=BankAccount Nothing Nothing (Just uid) Nothing details "JP Moresmau" (Just "Earth")
  acc2<-testMP $ createAccount acc1
  assertBool $ isJust $ baId acc2
  assertBool $ isJust $ baCreationDate acc2
  assertEqual iban $ atIBAN $ baDetails acc2
  if useBIC
    then assertEqual (Just bic) $ atBIC $ baDetails acc2
    else assertBool $ isJust $ atBIC $ baDetails acc2 -- MangoPay fills in the BIC.
  acc3<-testMP $ fetchAccount uid $ fromJust $ baId acc2
  assertEqual (baDetails acc2) (baDetails acc3)
  accs<-testMP $ listAccounts uid def Nothing
  assertBool $ acc3  `elem` (plData accs)
