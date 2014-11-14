{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | test documents
module Web.MangoPay.DocumentsTest where

import Web.MangoPay
import Web.MangoPay.TestUtils

import Data.Default
import Test.Framework
import Test.HUnit (Assertion)
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS

-- | test document API
test_Document :: Assertion
test_Document = do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  euser <- testMP $ getUser uid
  let d=Document Nothing Nothing Nothing IDENTITY_PROOF (Just CREATED) Nothing Nothing
  testEventTypes [KYC_CREATED,KYC_VALIDATION_ASKED] $ do
    d2<-testMP $ createDocument uid d
    assertBool (isJust $ dId d2)
    assertBool (isJust $ dCreationDate d2)
    assertEqual IDENTITY_PROOF (dType d2)
    assertEqual Light $ getKindOfAuthentication euser [d2]
    tf<-BS.readFile "data/test.jpg"
    -- document has to be in CREATED status
    testMP $ createPage uid (fromJust $ dId d2) tf
    tf2<-BS.readFile "data/test.png"
    testMP $ createPage uid (fromJust $ dId d2) tf2
    d3<-testMP $ modifyDocument uid d2{dStatus=Just VALIDATION_ASKED}
    assertEqual (Just VALIDATION_ASKED) (dStatus d3)
    assertEqual (dId d2) (dId d3)
    d4<-testMP $ fetchDocument uid (fromJust $ dId d2)
    assertEqual (Just VALIDATION_ASKED) (dStatus d4)
    docsUser <- testMP $ getAll $ listDocuments uid def def
    assertBool $ d3 `elem` docsUser
    docsUserI <- testMP $ getAll $ listDocuments uid def{dfType=Just IDENTITY_PROOF} def
    assertBool $ d3 `elem` docsUserI
    docsUserA <- testMP $ getAll $ listDocuments uid def{dfType=Just ADDRESS_PROOF} def
    assertBool $ not $ d3 `elem` docsUserA
    docsAll <- testMP $ getAll $ listAllDocuments def def
    assertBool $ d3 `elem` docsAll
    docsAllI <- testMP $ getAll $ listAllDocuments def{dfType=Just IDENTITY_PROOF} def
    assertBool $ d3 `elem` docsAllI
    docsAllA <- testMP $ getAll $ listAllDocuments def{dfType=Just ADDRESS_PROOF} def
    assertBool $ not $ d3 `elem` docsAllA
    return $ dId d2


-- | test type of authentication
test_KindOfAuthentication :: Assertion
test_KindOfAuthentication = do
  usL<-testMP $ listUsers def (Just $ Pagination 1 1)
  assertEqual 1 (length $ plData usL)
  let uid=urId $ head $ plData usL
  euser <- testMP $ getUser uid
  assertEqual Light $ getKindOfAuthentication euser []
