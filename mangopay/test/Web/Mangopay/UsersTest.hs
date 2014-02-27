{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.Mangopay.UsersTest where

import Web.Mangopay
import Web.Mangopay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (fromJust, isJust)

testNaturalUser :: NaturalUser
testNaturalUser=NaturalUser Nothing Nothing "jpmoresmau@gmail.com" "JP" "Moresmau" Nothing 11111 "FR" "FR" 
        (Just "Haskell contractor") Nothing Nothing Nothing Nothing 
--"1642966"
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
        eu<-testMP $ getUser (fromJust $ uId u)
        assertEqual (Left ue) eu
        us<-testMP $ listUsers (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ uId u)==) . urId) us)

testLegalUser :: LegalUser
testLegalUser = LegalUser Nothing Nothing "jpmoresmau@gmail.com" "JP Moresmau" Business Nothing
        "JP" "Moresmau" Nothing Nothing 222222 "FR" "FR" Nothing Nothing Nothing Nothing 
        
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
        el<-testMP $ getUser (fromJust $ lId l)
        assertEqual (Right le) el
        us<-testMP $ listUsers  (Just $ Pagination 1 100)
        assertEqual 1 (length $ filter (((fromJust $ lId l)==) . urId) us)
        