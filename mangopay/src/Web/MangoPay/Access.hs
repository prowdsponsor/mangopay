{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
-- | Obtaining credentials and access tokens.
module Web.MangoPay.Access
  ( createCredentialsSecret
  , oauthLogin
  ) where

import Control.Applicative ((<$>))
import Data.Maybe (isNothing)
import Data.Text
import Network.HTTP.Conduit (applyBasicAuth)
import Web.MangoPay.Monad
import Web.MangoPay.Types

import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT


-- | Populate the passphrase for our clientId IFF we don't have one.
createCredentialsSecret :: MPUsableMonad m => MangoPayT m Credentials
createCredentialsSecret = do
  creds <- getCreds
  if isNothing $ cClientSecret creds
    then postExchange "/v2/clients" Nothing creds
    else return creds

-- | Login with given user name and password.  Returns the OAuth
-- token that can be used to generate the opaque AccessToken and
-- carries the expiration delay.
oauthLogin :: MPUsableMonad m => Text -> Text -> MangoPayT m OAuthToken
oauthLogin user pass = do
  let query = [("grant_type", Just "client_credentials")] :: HT.Query
  req <- applyBasicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 pass) <$>
         getPostRequest "/v2/oauth/token" Nothing query
  getJSONResponse req
