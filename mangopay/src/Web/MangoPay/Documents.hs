{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
-- | handle documents and pages
module Web.MangoPay.Documents where


import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS

import qualified Data.Text.Encoding as TE

-- | create or edit a document
storeDocument ::  (MPUsableMonad m) => AnyUserID -> Document -> AccessToken -> MangoPayT m Document
storeDocument uid d at= 
        case dId d of
                Nothing-> do
                        url<-getClientURLMultiple ["/users/",uid,"/KYC/documents/"]
                        postExchange url (Just at) d
                Just i-> do
                        url<-getClientURLMultiple ["/users/",uid,"/KYC/documents/",i]
                        putExchange url (Just at) d
                

-- | fetch a document from its ID
fetchDocument :: (MPUsableMonad m) => AnyUserID -> DocumentID -> AccessToken -> MangoPayT m Document
fetchDocument uid did at=do
        url<-getClientURLMultiple ["/users/",uid,"/KYC/documents/",did]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req 

-- | create a page
--  note that per the MangoPay API the document HAS to be in CREATED status
-- should we check it here? Since MangoPay returns a 500 Internal Server Error if the document is in another status...
storePage :: (MPUsableMonad m) => AnyUserID -> DocumentID -> BS.ByteString -> AccessToken -> MangoPayT m ()
storePage uid did contents at=do
  let val=object ["File" .= TE.decodeLatin1 (BS.encode contents)]
  url<-getClientURLMultiple ["/users/",uid,"/KYC/documents/",did,"/pages"]
  postNoReply url (Just at) val

-- | ID of a document
type DocumentID = Text

-- | type of the document
data DocumentType= IDENTITY_PROOF -- ^ For legal and natural users
  | REGISTRATION_PROOF -- ^ Only for legal users
  | ARTICLES_OF_ASSOCIATION -- ^ Only for legal users
  | SHAREHOLDER_DECLARATION -- ^ Only for legal users
  | ADDRESS_PROOF -- ^ For legal and natural users
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON DocumentType where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON DocumentType where
        parseJSON (String s)=pure $ read $ unpack s
        parseJSON _ =fail "DocumentType"

-- | status of a document
data DocumentStatus=CREATED
  | VALIDATION_ASKED
  | VALIDATED
  | REFUSED 
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON DocumentStatus where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON DocumentStatus where
        parseJSON (String s)=pure $ read $ unpack s
        parseJSON _ =fail "DocumentStatus"

-- | a document
data Document = Document {
  dId :: Maybe DocumentID
  ,dCreationDate :: Maybe POSIXTime
  ,dTag :: Maybe Text -- ^  custom data for client
  ,dType :: DocumentType
  ,dStatus :: Maybe DocumentStatus
  ,dRefusedReasonType :: Maybe Text
  ,dRefusedReasonMessage :: Maybe Text
  } deriving (Show,Ord,Eq,Typeable)
  
 
-- | to json as per MangoPay format        
instance ToJSON Document where
        toJSON d=object ["Tag" .= dTag d,
          "Type" .= dType d,"Status" .= dStatus d]

-- | from json as per MangoPay format 
instance FromJSON Document where
        parseJSON (Object v) =Document <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "Type" <*>
                         v .: "Status" <*>
                         v .:? "RefusedReasonType" <*>
                         v .:? "RefusedReasonMessage"
        parseJSON _=fail "Document"
        
        
        
        