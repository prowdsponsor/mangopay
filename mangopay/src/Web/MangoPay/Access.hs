{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
-- | access methods for login, creating clients...
module Web.MangoPay.Access
(
createCredentialsSecret
,oauthLogin
)
where

import Web.MangoPay.Monad
import Web.MangoPay.Types

import Data.Text

import Network.HTTP.Conduit (applyBasicAuth)
import Control.Monad (liftM)
import qualified Network.HTTP.Types as HT
import qualified Data.Text.Encoding as TE
import Data.Maybe (isNothing)

-- | populate the passphrase for our clientId IFF we don't have one
createCredentialsSecret ::  (MPUsableMonad m) => MangoPayT m Credentials
createCredentialsSecret =do
        creds<- getCreds
        if isNothing $ cClientSecret creds
                then postExchange "/v2/clients" Nothing creds
                else return creds

-- | login with given user name and password
-- returns the OAuth token that can be used to generate the opaque AccessToken and carries the expiration delay
oauthLogin :: (MPUsableMonad m) => Text -> Text -> MangoPayT m OAuthToken
oauthLogin user pass = do
        req<- liftM (applyBasicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 pass)) $ getPostRequest "/v2/oauth/token" Nothing ([("grant_type",Just "client_credentials")]::HT.Query)
        getJSONResponse req
