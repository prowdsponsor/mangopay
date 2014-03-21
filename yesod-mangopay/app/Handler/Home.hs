{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Yesod.MangoPay
import Web.MangoPay

-- | Home page
getHomeR :: Handler Html
getHomeR = do
    users<-runYesodMPTToken $ listUsers Nothing
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome to the MangoPay demo application!"
        $(widgetFile "homepage")

