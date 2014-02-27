{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.Mangopay.UsersTest where

import Web.Mangopay
import Web.Mangopay.TestUtils

import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (fromJust, isJust)

testNaturalUser :: NaturalUser
testNaturalUser=NaturalUser "jpmoresmau@gmail.com" "JP" "Moresmau" Nothing 11111 "FR" "FR" 
        (Just "Haskell contractor") Nothing Nothing Nothing Nothing Nothing Nothing
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

testLegalUser :: LegalUser
testLegalUser = LegalUser "jpmoresmau@gmail.com" "JP Moresmau" Business Nothing
        "JP" "Moresmau" Nothing Nothing 222222 "FR" "FR" Nothing Nothing Nothing Nothing Nothing Nothing
        
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
        
        