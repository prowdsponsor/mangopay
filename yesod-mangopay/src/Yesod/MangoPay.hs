{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,TemplateHaskell, RankNTypes, ConstraintKinds, ScopedTypeVariables #-}
-- | typeclasses and helpers to access MangoPay from Yesod
module Yesod.MangoPay where

import Web.MangoPay

import qualified Yesod.Core as Y
import qualified Network.HTTP.Conduit as HTTP
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Text (Text)
import Data.IORef (IORef, readIORef, writeIORef)

import qualified Data.Map as M
import Control.Monad (void)
import qualified Control.Exception.Lifted as L

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
  
  -- | store the saved access token if we have one
  mpToken :: site -> IORef (Maybe MangoPayToken)

  
-- | Run a 'MangoPayT' action inside a 'Y.GHandler' using your credentials.
runYesodMPT ::
  (Y.MonadHandler m,MPUsableMonad m, Y.HandlerSite m ~ site, YesodMangoPay site) =>
  MangoPayT m a -> m a
runYesodMPT act = do
  site <- Y.getYesod
  let creds   = mpCredentials site
      manager = mpHttpManager site
      apoint  = if mpUseSandbox site then Sandbox else Production
  runMangoPayT creds manager apoint act

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
 
-- | get the currently stored token if we have one and it's valid, or Nothing otherwise
getTokenIfValid ::   (YesodMangoPay site,Y.MonadResource m) => site -> m (Maybe MangoPayToken)
getTokenIfValid site=do
  mt<-Y.liftIO $ readIORef $ mpToken site
  case mt of
    Nothing-> return Nothing
    Just t->do
      v<-isTokenValid t
      return $ if v then Just t else Nothing

-- | get a valid token, which could be one we had from before, or a new one
getValidToken ::  (YesodMangoPay site,MPUsableMonad m) => site -> m (Maybe AccessToken)
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
        Nothing -> fail "getValidToken: You need to provide the cClientSecret on the mpCredentials."
        Just secret-> do
          oat<-runMangoPayT creds manager apoint $
                  oauthLogin (cClientID creds) secret   
          ct<-Y.liftIO getCurrentTime
          -- oaExpires is in second, remove one minute for safety
          let expires=addUTCTime (fromIntegral (oaExpires oat - 60)) ct
          let at=toAccessToken oat
          Y.liftIO $ writeIORef (mpToken site) (Just $ MangoPayToken at expires)
          return $ Just at
          
-- | same as 'runYesodMPT': runs a MangoPayT computation, but tries to reuse the current token if valid
runYesodMPTToken ::
  (Y.MonadHandler m,MPUsableMonad m, Y.HandlerSite m ~ site, YesodMangoPay site) =>
  (AccessToken -> MangoPayT m a) -> m a
runYesodMPTToken act = do
  site <- Y.getYesod
  vt<-getValidToken site
  case vt of
    Nothing -> fail "runYesodMPTToken: Could not obtain access token."
    Just ac-> runYesodMPT $ act ac


-- | register callbacks for each event type on the same url
-- mango pay does not let register two hooks for the same event, so we replace existing ones
registerAllMPCallbacks ::  (Y.MonadHandler m,MPUsableMonad m, Y.HandlerSite m ~ site, YesodMangoPay site) =>
  Y.Route (Y.HandlerSite m)-> m ()
registerAllMPCallbacks rt=do
  render<-Y.getUrlRender
  let url=render rt
  $(Y.logInfo) url
  runYesodMPTToken $ \at-> do
    -- get all hooks at once
    hooks<-getAll listHooks at
    let existing=foldr (\h s->M.insert (hEventType h) h s) M.empty hooks
    mapM_ (registerIfAbsent url at existing) [minBound..maxBound]
  where
    registerIfAbsent url at existing evt=do
        let mh=case M.lookup evt existing of
                Nothing->Just $ Hook Nothing Nothing Nothing url Enabled Nothing evt 
                Just h2->if hUrl h2 /= url then Just h2{hUrl = url} else Nothing
        case mh of 
          Just h->do                           
            Y.liftIO $ print h
            void $ storeHook h at
          _-> return ()

-- | register a call back using the given route
registerMPCallback :: (Y.MonadHandler m,MPUsableMonad m, Y.HandlerSite m ~ site, YesodMangoPay site) =>
  Y.Route (Y.HandlerSite m)-> EventType -> Maybe Text -> m (AccessToken -> MangoPayT m Hook)
registerMPCallback rt et mtag=do
  render<-Y.getUrlRender
  let h=Hook Nothing Nothing mtag (render rt) Enabled Nothing et 
  return $ storeHook h
    
-- | parse a event from a notification callback    
parseMPNotification :: (Y.MonadHandler m, Y.HandlerSite m ~ site, YesodMangoPay site) => m Event
parseMPNotification = do
  req<-Y.getRequest
  let mevt=eventFromQueryStringT $ Y.reqGetParams req
  case mevt of
    Just evt->return evt
    Nothing->fail "parseMPNotification: could not parse Event"
  
-- | catches any exception that the MangoPay library may throw and deals with it in a error handler
catchMP :: forall (m :: * -> *) a.
             Y.MonadBaseControl IO m =>
             m a -> (MpException -> m a) -> m a
catchMP=L.catch
