{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Web.MangoPay.AccessTest
import {-@ HTF_TESTS @-} Web.MangoPay.UsersTest
import {-@ HTF_TESTS @-} Web.MangoPay.WalletsTest
import {-@ HTF_TESTS @-} Web.MangoPay.DocumentsTest

-- | test entry point
main :: IO()
main = htfMain htf_importedTests
