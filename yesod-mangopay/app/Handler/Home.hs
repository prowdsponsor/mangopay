{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- | home and events pages
module Handler.Home where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.IORef (modifyIORef, readIORef)
import Control.Monad (when)

-- | Home page
getHomeR :: Handler Html
getHomeR = do
    pg<-getPagination
    usersL<-runYesodMPTToken $ listUsers (ByCreationDate DESC) pg
    -- pagination links
    let (previous,next)=getPaginationNav pg usersL
    let users=plData usersL
    defaultLayout $ do
        setTitleI MsgHello
        $(widgetFile "homepage")

-- | notification callback page
getMPHookR :: Handler Text
getMPHookR = do
  evt<-parseMPNotification
  site <- getYesod
  ok <- runYesodMPTToken $ checkEvent evt
  when ok $
      -- prepend event to list
      liftIO $ modifyIORef (appEvents site) (evt :)
  -- send simple response
  sendResponse ("ok"::Text)

-- | list received events
getMPEventsR :: Handler Html
getMPEventsR = do
  site <- getYesod
  events<-liftIO $ readIORef  (appEvents site)
  defaultLayout $ do
      setTitleI MsgTitleEvents
      $(widgetFile "events")
