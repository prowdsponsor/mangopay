{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
-- | useful types and simple accessor functions
module Web.Mangopay.Types where


import Control.Applicative
import Control.Exception.Base (Exception)
import Data.Text as T
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Aeson

import qualified Data.Text.Encoding as TE

-- | the Mangopay access point
data AccessPoint = Sandbox | Production | Custom ByteString
        deriving (Show,Read,Eq,Ord,Typeable)
        
-- | get the real url for the given access point
getAccessPointURL :: AccessPoint -> ByteString
getAccessPointURL Sandbox="api.sandbox.mangopay.com"
getAccessPointURL Production="api.mangopay.com"
getAccessPointURL (Custom bs)=bs

-- | the app credentials
data Credentials = Credentials {
  cClientID :: Text -- ^ client id
  ,cName :: Text -- ^ the name
  ,cEmail :: Text -- ^ the email
  ,cClientSecret :: Text -- ^ client secret
  }
  deriving (Show,Read,Eq,Ord,Typeable)
      
-- | to json as per Mangopay format    
instance ToJSON Credentials  where
    toJSON c=object ["ClientId" .= cClientID c, "Name" .= cName c , "Email" .= cEmail c] 

-- | from json as per Mangopay format
instance FromJSON Credentials where
    parseJSON (Object v) =Credentials <$>
                         v .: "ClientId" <*>
                         v .: "Name" <*>
                         v .: "Email" <*>
                         v .: "Passphrase"
    parseJSON _= fail "Credentials"      
      
-- | get client id in ByteString form
clientIDBS :: Credentials -> ByteString
clientIDBS=TE.encodeUtf8 . cClientID

-- | get client secret in ByteString form
clientSecretBS :: Credentials -> ByteString
clientSecretBS=TE.encodeUtf8 . cClientSecret

-- | the access token is simply a Text
newtype AccessToken=AccessToken ByteString
    deriving (Eq, Ord, Read, Show, Typeable)
 
        
-- | the oauth token returned after authentication
data OAuthToken = OAuthToken {
  oaAccessToken :: Text -- ^ the access token
  ,oaTokenType :: Text -- ^ the token type
  ,oaExpires :: Int -- ^ expiration
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Mangopay format
instance ToJSON OAuthToken  where
    toJSON oa=object ["access_token" .= oaAccessToken oa, "token_type" .= oaTokenType oa, "expires_in" .= oaExpires oa] 

-- | from json as per Mangopay format        
instance FromJSON OAuthToken where
    parseJSON (Object v) =OAuthToken <$>
                         v .: "access_token" <*>
                         v .: "token_type" <*>
                         v .: "expires_in" 
    parseJSON _= fail "OAuthToken"        
 
-- | build the access token from the OAuthToken       
toAccessToken ::  OAuthToken -> AccessToken
toAccessToken  oa=AccessToken $ TE.encodeUtf8 $ T.concat [oaTokenType oa, " ",oaAccessToken oa]
        
-- | an exception that a call to Mangopay may throw
data MpException = JSONException String -- ^ JSON parsingError
  | MpAppException MpError -- ^ application exception
  deriving (Show,Typeable)

-- | make our exception type a normal exception  
instance Exception MpException 


-- | an error returned to us by Mangopay
data MpError = MpError {
  igeID :: Text
  ,igeType :: Text
  ,igeMessage :: Text
  ,igeDate :: Maybe POSIXTime
  }
  deriving (Show,Eq,Ord,Typeable)
 
 
  
-- | from json as per Mangopay format
instance FromJSON MpError where
    parseJSON (Object v) = MpError <$>
                         v .: "Id" <*>
                         v .: "Type" <*>
                         v .: "Message" <*>
                         v .: "Date"
    parseJSON _= fail "MpError"
    
instance FromJSON POSIXTime where
    parseJSON n@(Number _)=(fromIntegral . (round::Double -> Integer)) <$> parseJSON n
    parseJSON _ = fail "POSIXTime"
    
-- | to json as per Mangopay format
instance ToJSON POSIXTime  where
    toJSON pt=toJSON (round pt :: Integer)
    