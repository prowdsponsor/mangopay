{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.UsersTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (fromJust, isJust)

import Data.CountryCodes (CountryCode(FR))
import Data.Default

testNaturalUser :: NaturalUser
testNaturalUser=NaturalUser Nothing Nothing "jpmoresmau@gmail.com" "JP" "Moresmau" Nothing (MpTime 11111) FR FR
        (Just "Haskell contractor") (Just IncomeRange2) Nothing Nothing Nothing

test_NaturalUser :: Assertion
test_NaturalUser = do
        u<-testMP $ createNaturalUser testNaturalUser
        assertBool (isJust $ uId u)
        assertBool (isJust $ uCreationDate u)
        uf<-testMP $ fetchNaturalUser (fromJust $ uId u)
        assertEqual (uEmail testNaturalUser) (uEmail uf)
        assertEqual Nothing (uAddress uf)
        ue<-testMP $ modifyNaturalUser (uf{uAddress=Just "St Guilhem"})
        assertEqual (Just "St Guilhem") (uAddress ue)
        assertEqual (Just IncomeRange2) (uIncomeRange ue)
        eu<-testMP $ getUser (fromJust $ uId u)
        assertEqual (Left ue) eu
        usL<-testMP $ listUsers def (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ uId u)==) . urId) $ plData usL)
        assertEqual (fromJust $ uId u) (getExistingUserId $ Left u)

testLegalUser :: LegalUser
testLegalUser = LegalUser Nothing Nothing "jpmoresmau@gmail.com" "JP Moresmau" Business Nothing
        "JP" "Moresmau" (Just "my house") Nothing (MpTime 222222) FR FR Nothing Nothing Nothing Nothing

test_LegalUser :: Assertion
test_LegalUser = do
        l<-testMP $ createLegalUser testLegalUser
        assertBool (isJust $ lId l)
        assertBool (isJust $ lCreationDate l)
        lf<-testMP $ fetchLegalUser (fromJust $ lId l)
        assertEqual (lEmail testLegalUser) (lEmail lf)
        assertEqual Nothing (lHeadquartersAddress lf)
        le<-testMP $ modifyLegalUser (lf{lHeadquartersAddress=Just "St Guilhem"})
        assertEqual (Just "St Guilhem") (lHeadquartersAddress le)
        assertEqual "Moresmau" (lLegalRepresentativeLastName le)
        assertEqual (Just "my house") (lLegalRepresentativeAddress le) -- works since Zebre <http://docs.mangopay.com/release-zebre/>
        assertEqual Nothing (lProofOfRegistration le)
        assertEqual Nothing (lShareholderDeclaration le)
        el<-testMP $ getUser (fromJust $ lId l)
        assertEqual (Right le) el
        usL<-testMP $ listUsers def (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ lId l)==) . urId) $ plData usL)
        assertEqual (fromJust $ lId l) (getExistingUserId $ Right l)

test_PaginationUsers :: Assertion
test_PaginationUsers = do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  --print usL
  assertEqual 1 (length $ plData usL)
  assertEqual 2 (plItemCount usL)
  assertEqual 2 (plPageCount usL)
  usL2<-testMP $ listUsers def (Just $ Pagination 1 10)
  assertEqual 2 (length $ plData usL2)
  assertEqual 2 (plItemCount usL2)
  assertEqual 1 (plPageCount usL2)
  us<-testMP $ getAll $ listUsers def
  assertEqual 2 (length us)

test_OrderUsers :: Assertion
test_OrderUsers = do
  usL<-testMP $ listUsers (ByCreationDate ASC) (Just $ Pagination 1 10)
  assertEqual 2 (length $ plData usL)
  let idsAsc = map urId $ plData usL
  usLd<-testMP $ listUsers (ByCreationDate DESC) (Just $ Pagination 1 10)
  assertEqual 2 (length $ plData usLd)
  let idsDesc = map urId $ plData usLd
  assertEqual idsAsc $ reverse idsDesc
