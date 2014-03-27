{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.UsersTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (fromJust, isJust)

testNaturalUser :: NaturalUser
testNaturalUser=NaturalUser Nothing Nothing "jpmoresmau@gmail.com" "JP" "Moresmau" Nothing 11111 "FR" "FR" 
        (Just "Haskell contractor") (Just IncomeRange2) Nothing Nothing Nothing 

test_NaturalUser :: Assertion
test_NaturalUser = do
        u<-testMP $ storeNaturalUser testNaturalUser
        assertBool (isJust $ uId u)
        assertBool (isJust $ uCreationDate u)
        uf<-testMP $ fetchNaturalUser (fromJust $ uId u)
        assertEqual (uEmail testNaturalUser) (uEmail uf)
        assertEqual Nothing (uAddress uf)
        ue<-testMP $ storeNaturalUser (uf{uAddress=Just "St Guilhem"})
        assertEqual (Just "St Guilhem") (uAddress ue)
        assertEqual (Just IncomeRange2) (uIncomeRange ue)
        eu<-testMP $ getUser (fromJust $ uId u)
        assertEqual (Left ue) eu
        usL<-testMP $ listUsers (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ uId u)==) . urId) $ plData usL)

testLegalUser :: LegalUser
testLegalUser = LegalUser Nothing Nothing "jpmoresmau@gmail.com" "JP Moresmau" Business Nothing
        "JP" "Moresmau" (Just "my house") Nothing 222222 "FR" "FR" Nothing Nothing Nothing Nothing
        
test_LegalUser :: Assertion
test_LegalUser = do
        l<-testMP $ storeLegalUser testLegalUser
        assertBool (isJust $ lId l)
        assertBool (isJust $ lCreationDate l)
        lf<-testMP $ fetchLegalUser (fromJust $ lId l)
        assertEqual (lEmail testLegalUser) (lEmail lf)
        assertEqual Nothing (lHeadquartersAddress lf)
        le<-testMP $ storeLegalUser (lf{lHeadquartersAddress=Just "St Guilhem"})
        assertEqual (Just "St Guilhem") (lHeadquartersAddress le)
        assertEqual "Moresmau" (lLegalRepresentativeLastName le)
        assertEqual (Just "my house") (lLegalRepresentativeAddress le) -- this is lost, see https://mangopay.desk.com/customer/portal/questions/5980417-legalrepresentativeaddress-in-legaluser-api
        assertEqual Nothing (lProofOfRegistration le)
        assertEqual Nothing (lShareholderDeclaration le)
        el<-testMP $ getUser (fromJust $ lId l)
        assertEqual (Right le) el
        usL<-testMP $ listUsers  (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ lId l)==) . urId) $ plData usL)
   
test_PaginationUsers :: Assertion
test_PaginationUsers = do
  usL<-testMP $ listUsers (Just $ Pagination 1 1)
  print usL
  assertEqual 1 (length $ plData usL)
  assertEqual 2 (plItemCount usL)
  assertEqual 2 (plPageCount usL)
  usL2<-testMP $ listUsers (Just $ Pagination 1 10)
  assertEqual 2 (length $ plData usL2)
  assertEqual 2 (plItemCount usL2)
  assertEqual 1 (plPageCount usL2)
  us<-testMP $ getAll listUsers
  assertEqual 2 (length us)