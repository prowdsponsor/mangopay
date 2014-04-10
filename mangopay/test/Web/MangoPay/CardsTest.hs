{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test cards
module Web.MangoPay.CardsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Maybe (isJust, isNothing, fromJust)
import Test.Framework
import Test.HUnit (Assertion)

import qualified Data.Text as T
import qualified Control.Exception.Lifted as L

-- | test a card registration using euro currency
test_CardEUR :: Assertion
test_CardEUR = doTestCard "EUR"

-- | test a card registration using dollar currency
test_CardUSD :: Assertion
test_CardUSD = doTestCard "USD"

-- | perform the actual test of card registration in the provided currency
doTestCard :: T.Text->Assertion
doTestCard curr=L.handle (\(e::MpException)->assertFailure (show e)) $ do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  let cr1=mkCardRegistration uid curr
  cr2<-testMP $ storeCardRegistration cr1
  assertBool (isJust $ crId cr2)
  assertBool (isJust $ crCreationDate cr2)
  assertBool (isJust $ crCardRegistrationURL cr2)
  assertBool (isJust $ crAccessKey cr2)
  assertBool (isJust $ crPreregistrationData cr2)  
  assertBool (isNothing $ crRegistrationData cr2)  
  assertBool (isNothing $ crCardId cr2)  
  cr3<-unsafeRegisterCard testCardInfo1 cr2
  assertBool (isJust $ crRegistrationData cr3)  
  cr4<-testMP $ storeCardRegistration cr3
  assertBool (isJust $ crCardId cr4)  
  let cid=fromJust $ crCardId cr4
  c<-testMP $ fetchCard cid
  assertEqual cid $ cId c
  assertBool $ not $ T.null $ cAlias c 
  assertBool $ not $ T.null $ cCardProvider c
  assertEqual (ciExpire testCardInfo1) (cExpirationDate c)
  --assertBool $ not $ T.null $ cExpirationDate c
  assertEqual UNKNOWN $ cValidity c
  assertBool $ cActive c
  assertEqual uid $ cUserId c
  assertEqual "CB_VISA_MASTERCARD" $ cCardType c
  cs<-testMP $ getAll $ listCards uid
  assertBool $ not $ null cs
  assertBool $ any (\ c1 -> cId c == cid) cs
  
 
  
  