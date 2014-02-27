{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | access methods for login, creating clients...
module Web.Mangopay.Access 
(
createCredentialsSecret
,oauthLogin
)
where

import Web.Mangopay.Monad
import Web.Mangopay.Types

import Data.Conduit
import Data.Text

import Network.HTTP.Conduit (applyBasicAuth)
import Control.Monad (liftM)
import qualified Network.HTTP.Types as HT
import qualified Data.Text.Encoding as TE
import Data.Maybe (isNothing)

-- | populate the passphrase for our clientId IFF we don't have one
createCredentialsSecret ::  (MonadBaseControl IO m, MonadResource m) => MangopayT m Credentials
createCredentialsSecret =do
        creds<- getCreds 
        if isNothing $ cClientSecret creds 
                then postExchange "/v2/clients" Nothing creds 
                else return creds

-- | login with given user name and password
oauthLogin :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> MangopayT m AccessToken
oauthLogin user pass = do
        req<- liftM (applyBasicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 pass)) $ getPostRequest "/v2/oauth/token" Nothing ([("grant_type",Just "client_credentials")]::HT.Query)
        t<-getJSONResponse req
        return $ toAccessToken t