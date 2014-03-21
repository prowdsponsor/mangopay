{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.IORef (modifyIORef, readIORef)

-- | Home page
getHomeR :: Handler Html
getHomeR = do
    users<-runYesodMPTToken $ listUsers Nothing
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome to the MangoPay demo application!"
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
      setTitle "MangoPay Events!"
      $(widgetFile "events")