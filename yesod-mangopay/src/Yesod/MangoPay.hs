{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
-- | typeclasses and helpers to access MangoPay from Yesod
module Yesod.MangoPay where

import Web.MangoPay

import qualified Yesod.Core as Y
import qualified Network.HTTP.Conduit as HTTP
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Network.HTTP.Types (internalServerError500)
import Data.Text (Text)

-- | The 'YesodMangoPay' class for foundation datatypes that
-- support running 'MangoPayT' actions.
class Y.Yesod site => YesodMangoPay site where
  -- | The credentials of your app.
  mpCredentials :: site -> Credentials

  -- | HTTP manager used for contacting MangoPay (may be the same
  -- as the one used for @yesod-auth@).
  mpHttpManager :: site -> HTTP.Manager

  -- | Use MangoPay's sandbox if @True@.  The default is @True@ for safety.
  mpUseSandbox :: site -> Bool
  mpUseSandbox _ = True

  
-- | Run a 'MangoPayT' action inside a 'Y.GHandler' using your
-- credentials and a newly created access token.
runYesodMPT ::
  (Y.MonadHandler m,Y.MonadBaseControl IO m, Y.HandlerSite m ~ site, YesodMangoPay site) =>
  (AccessToken -> MangoPayT m a) -> m a
runYesodMPT act = do
  site <- Y.getYesod
  let creds   = mpCredentials site
      msecret  = cClientSecret creds
      manager = mpHttpManager site
      apoint  = if mpUseSandbox site then Sandbox else Production
  case msecret of
    Nothing -> Y.sendResponseStatus internalServerError500 ("No secret provided"::Text)
    Just secret-> do
      oat<-runMangoPayT creds manager apoint $
              oauthLogin (cClientID creds) secret   
      runMangoPayT creds manager apoint (act $ toAccessToken oat)
  

-- | the MangoPay access token, valid for a certain time only
data MangoPayToken=MangoPayToken {
  mptToken :: AccessToken -- ^ opaque token
  ,mptExpires :: UTCTime -- ^ expiration date
  }

-- | is the given token still valid (True) or has it expired (False)?  
isTokenValid :: (Y.MonadResource m) =>  MangoPayToken -> m Bool
isTokenValid mpt=do
  ct<-Y.liftIO getCurrentTime
  return $ diffUTCTime (mptExpires mpt) ct > 0
 
-- | a class allowing storing the token  
class YesodMangoPay site => YesodMangoPayTokenHandler site where
  getToken :: (Y.MonadResource m) => site -> m (Maybe MangoPayToken)
  setToken :: (Y.MonadResource m) => site -> MangoPayToken -> m ()

-- | get the currently stored token if we have one and it's valid, or Nothing otherwise
getTokenIfValid ::   (YesodMangoPayTokenHandler site,Y.MonadResource m) => site -> m (Maybe MangoPayToken)
getTokenIfValid site=do
  mt<-getToken site
  case mt of
    Nothing-> return Nothing
    Just t->do
      v<-isTokenValid t
      return $ if v then Just t else Nothing

-- | get a valid token, which could be one we had from before, or a new one
getValidToken ::  (YesodMangoPayTokenHandler site,Y.MonadBaseControl IO m,Y.MonadResource m) => site -> m (Maybe AccessToken)
getValidToken site=do
  mt<-getTokenIfValid site
  case mt of
    Just t-> return $ Just $ mptToken t
    Nothing->do
      let creds   = mpCredentials site
          msecret  = cClientSecret creds
          manager = mpHttpManager site
          apoint  = if mpUseSandbox site then Sandbox else Production
      case msecret of
        Nothing -> return Nothing
        Just secret-> do
          oat<-runMangoPayT creds manager apoint $
                  oauthLogin (cClientID creds) secret   
          ct<-Y.liftIO getCurrentTime
          -- oaExpires is in second, remove one minute for safety
          let expires=addUTCTime (fromIntegral (oaExpires oat - 60)) ct
          let at=toAccessToken oat
          setToken site (MangoPayToken at expires)
          return $ Just at
          
-- | same as 'runYesodMPT': runs a MangoPayT computation, but tries to reuse the current token if valid
runYesodMPTToken ::
  (Y.MonadHandler m,Y.MonadBaseControl IO m, Y.HandlerSite m ~ site, YesodMangoPayTokenHandler site) =>
  (AccessToken -> MangoPayT m a) -> m a
runYesodMPTToken act = do
  site <- Y.getYesod
  let creds   = mpCredentials site
      manager = mpHttpManager site
      apoint  = if mpUseSandbox site then Sandbox else Production
  vt<-getValidToken site
  case vt of
    Nothing -> Y.sendResponseStatus internalServerError500 ("Could not obtain access token"::Text)
    Just ac-> runMangoPayT creds manager apoint $ act ac
    