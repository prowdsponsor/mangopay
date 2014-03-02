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
  us<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length us)
  let uid=urId $ head us
  let w=Wallet Nothing Nothing (Just "custom") [uid] "my wallet" "EUR" Nothing 
  w2<-testMP $ storeWallet w
  assertBool (isJust $ wId w2)
  let bw1=mkBankWire uid uid (fromJust $ wId w2) (Amount "EUR" 100) (Amount "EUR" 1)
  bw2<-testMP $ storeBankWire bw1
  assertBool (isJust $ bwId bw2)
  assertBool (isJust $ bwBankAccount bw2)
  bw3<-testMP $ fetchBankWire (fromJust $ bwId bw2)
  assertEqual (bwId bw2) (bwId bw3)