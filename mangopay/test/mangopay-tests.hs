{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | entry module for tests
module Main where


import Test.Framework
import Test.HUnit (Assertion)

-- Order matters!
import {-@ HTF_TESTS @-} Web.MangoPay.SimpleTest
import {-@ HTF_TESTS @-} Web.MangoPay.UsersTest
import {-@ HTF_TESTS @-} Web.MangoPay.WalletsTest
import {-@ HTF_TESTS @-} Web.MangoPay.DocumentsTest
import {-@ HTF_TESTS @-} Web.MangoPay.PayinsTest
import {-@ HTF_TESTS @-} Web.MangoPay.CardsTest
import {-@ HTF_TESTS @-} Web.MangoPay.RefundsTest
import {-@ HTF_TESTS @-} Web.MangoPay.AccountsTest
import {-@ HTF_TESTS @-} Web.MangoPay.PayoutsTest

import Web.MangoPay.TestUtils


-- | Test entry point.
main :: IO ()
main =
  withMangoPayTestUtils $ \_ _ _ ->
  htfMain $ htf_importedTests ++ [htf_thisModulesTests]


-- | Test there are no unprocessed events.
test_Final :: Assertion
test_Final = ensureNoEvents 5
