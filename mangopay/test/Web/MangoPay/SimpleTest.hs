{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Web.MangoPay.SimpleTest where

import Web.MangoPay

import qualified Data.Text as T
import Test.Framework
import Test.HUnit (Assertion)
import Data.Either (rights)

-- | test serialization of card expiration
test_CardExpiration :: Assertion
test_CardExpiration = do
  let ece1=readCardExpiration "0225"
  assertBool $ not $ null $ rights [ece1]
  let Right (ce1,t1)=ece1
  assertBool $ T.null t1
  assertEqual (CardExpiration 2 25) ce1
  assertEqual "0225" $ writeCardExpiration ce1
  let ece2=readCardExpiration "1503"
  assertBool $ not $ null $ rights [ece2]
  let Right (ce2,t2)=ece2
  assertBool $ T.null t2
  assertEqual (CardExpiration 15 3) ce2
  assertEqual "1503" $ writeCardExpiration ce2
  let ce3 = CardExpiration 10 2034
  assertEqual "1034" $ writeCardExpiration ce3


-- | test income ranges
test_IncomeRanges :: Assertion
test_IncomeRanges = mapM_ testIncomeRange [minBound .. maxBound]

-- | test one income range
testIncomeRange :: IncomeRange -> Assertion
testIncomeRange ir=do
  let (Amount c1 a1,Amount c2 a2) = incomeBounds ir
  assertEqual "EUR" c1
  assertEqual "EUR" c2
  if a2 > (-1)
    then do
      let mid = div (a1+a2) 2
      assertEqual ir $ incomeRange (Amount c1 mid)
    else do
      assertEqual IncomeRange6 ir
      let mid = a1 * 2
      assertEqual ir $ incomeRange (Amount c1 mid)
