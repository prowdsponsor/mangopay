{-# LANGUAGE TupleSections, OverloadedStrings #-}
-- | home and events pages
module Handler.Home where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.IORef (modifyIORef, readIORef)

-- | Home page
getHomeR :: Handler Html
getHomeR = do
    pg<-getPagination
    usersL<-runYesodMPTToken $ listUsers pg
    -- pagination links
    let (previous,next)=getPaginationNav pg usersL
    let users=plData usersL
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgHello
        $(widgetFile "homepage")

-- | notification callback page
getMPHookR :: Handler Text
getMPHookR = do
  evt<-parseMPNotification
  site <- getYesod
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
      aDomId <- newIdent
      setTitleI MsgTitleEvents
      $(widgetFile "events")