{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Web.MangoPay.UsersTest
import {-@ HTF_TESTS @-} Web.MangoPay.WalletsTest

-- | test entry point
main :: IO()
main = htfMain htf_importedTests
