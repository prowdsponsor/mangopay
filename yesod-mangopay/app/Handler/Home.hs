{-# LANGUAGE TupleSections, OverloadedStrings #-}
-- | home and events pages
module Handler.Home where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.IORef (modifyIORef, readIORef)
import Data.Maybe
import Control.Monad (liftM)
import Data.Text.Read (decimal)

-- | Home page
getHomeR :: Handler Html
getHomeR = do
    -- poor man's parameter handling...
    pg<-liftM (fromMaybe "1") $ lookupGetParam "page"
    let Right (i,_)=decimal pg
    usersL<-runYesodMPTToken $ listUsers (Just $ Pagination i 10)
    -- pagination links
    let next=if plPageCount usersL > i
              then Just (i+1)
              else Nothing
    let previous=if i>1
              then Just (i-1)
              else Nothing
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