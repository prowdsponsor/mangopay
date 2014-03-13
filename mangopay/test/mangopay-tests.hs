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
import {-@ HTF_TESTS @-} Web.MangoPay.RefundsTest

import Web.MangoPay.TestUtils

import Control.Exception (bracket)
import Network.HTTP.Conduit as H
import Control.Concurrent (killThread)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Test.HUnit (Assertion)
import Control.Monad (liftM)

-- | test entry point
main :: IO()
main = H.withManager (\mgr->liftIO $ do
    hook<-getHookEndPoint
    res<-newReceivedEvents
    -- initial state
    modifyIORef testState (\ts->ts{tsManager=mgr,tsHookEndPoint=hook,tsReceivedEvents=res})
    bracket 
          (startHTTPServer (hepPort hook) res)
          killThread
          (\_->htfMain $ htf_importedTests ++ [htf_thisModulesTests])
    )

-- | test there are no unprocessed events    
test_Final :: Assertion
test_Final=do
  res<-liftM tsReceivedEvents $ readIORef testState         
  evts<-popReceivedEvents res
  assertEqual [] evts
  