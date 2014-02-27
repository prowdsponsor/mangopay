{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Web.Mangopay.UsersTest
import {-@ HTF_TESTS @-} Web.Mangopay.WalletsTest

-- | test entry point
main :: IO()
main = htfMain htf_importedTests
