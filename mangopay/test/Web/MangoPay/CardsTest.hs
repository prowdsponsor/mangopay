{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test cards
module Web.MangoPay.CardsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Maybe (isJust, isNothing)
import Test.Framework
import Test.HUnit (Assertion)

test_Card :: Assertion
test_Card = do
  us<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length us)
  let uid=urId $ head us
  let cr1=mkCardRegistration uid "EUR"
  cr2<-testMP $ storeCardRegistration cr1
  assertBool (isJust $ crId cr2)
  assertBool (isJust $ crCreationDate cr2)
  assertBool (isJust $ crCardRegistrationURL cr2)
  assertBool (isJust $ crAccessKey cr2)
  assertBool (isJust $ crPreregistrationData cr2)  
  assertBool (isNothing $ crRegistrationData cr2)  
  assertBool (isNothing $ crCardId cr2)  
  cr3<-testMP $ (\_->registerCard testCardInfo1 cr2)
  assertBool (isJust $ crRegistrationData cr3)  
  cr4<-testMP $ storeCardRegistration cr3
  assertBool (isJust $ crCardId cr4)  
  
--  N°: 4970100000000154
--Expiry: Any date in the future
--CSC: 123