{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | entry module for tests
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Web.MangoPay.AccessTest
import {-@ HTF_TESTS @-} Web.MangoPay.UsersTest
import {-@ HTF_TESTS @-} Web.MangoPay.WalletsTest
import {-@ HTF_TESTS @-} Web.MangoPay.DocumentsTest
import {-@ HTF_TESTS @-} Web.MangoPay.PayinsTest
import {-@ HTF_TESTS @-} Web.MangoPay.CardsTest

-- | test entry point
main :: IO()
main = htfMain htf_importedTests
