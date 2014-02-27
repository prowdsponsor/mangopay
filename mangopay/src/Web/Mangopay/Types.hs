{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
-- | useful types and simple accessor functions
module Web.Mangopay.Types where


import Control.Applicative
import Control.Exception.Base (Exception)
import Data.Text as T
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Aeson
import Data.Default

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as UTF8

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
  ,cClientSecret :: Maybe Text -- ^ client secret, maybe be Nothing if we haven't generated it
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
    
-- | Pagination info for searches
-- <http://docs.mangopay.com/api-references/pagination/>
data Pagination = Pagination {
        pPage :: Integer
        ,pPerPage :: Integer
        }
        deriving (Show,Read,Eq,Ord,Typeable)
        
instance Default Pagination where
        def=Pagination 1 10

-- | get pagination attributes for query 
paginationAttributes :: Maybe Pagination -> [(ByteString,Maybe ByteString)]
paginationAttributes (Just p)=["page" ?+ pPage p, "per_page" ?+ pPerPage p]
paginationAttributes _=[]


-- | simple class used to hide the serialization of parameters ansd simplify the calling code  
class ToHtQuery a where
  (?+) :: ByteString -> a -> (ByteString,Maybe ByteString)

instance ToHtQuery Double where
  n ?+ d=n ?+ show d

instance ToHtQuery (Maybe Double) where
  n ?+ d=n ?+ fmap show d

instance ToHtQuery Integer where
  n ?+ d=n ?+ show d
 
instance ToHtQuery (Maybe Integer) where
  n ?+ d=n ?+ fmap show d
    
instance ToHtQuery (Maybe POSIXTime) where
  n ?+ d=n ?+ fmap (show . (round :: POSIXTime -> Integer)) d
  
instance ToHtQuery (Maybe T.Text) where
  n ?+ d=(n,fmap TE.encodeUtf8 d)

instance ToHtQuery T.Text where
  n ?+ d=(n,Just $ TE.encodeUtf8 d)
  

instance ToHtQuery (Maybe String) where
  n ?+ d=(n,fmap UTF8.fromString d)  

instance ToHtQuery String where
  n ?+ d=(n,Just $ UTF8.fromString d)  