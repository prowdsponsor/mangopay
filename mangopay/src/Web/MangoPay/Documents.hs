{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
-- | handle documents and pages
module Web.MangoPay.Documents where


import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.ByteString (ByteString)
import Data.Default
import Data.Text hiding (any)
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | create a document
createDocument ::  (MPUsableMonad m) => AnyUserId -> Document -> AccessToken -> MangoPayT m Document
createDocument uid d = createGeneric path d
        where path = BS.concat ["/users/",TE.encodeUtf8 uid,"/KYC/documents/"]


modifyDocument ::  (MPUsableMonad m) => AnyUserId -> Document -> AccessToken -> MangoPayT m Document
modifyDocument uid d = modifyGeneric path d dId
        where path = T.concat ["/users/", uid, "/KYC/documents/"]


-- | fetch a document from its Id
fetchDocument :: (MPUsableMonad m) => AnyUserId -> DocumentId -> AccessToken -> MangoPayT m Document
fetchDocument uid = fetchGeneric path
        where path = T.concat ["/users/",uid,"/KYC/documents/"]

-- | create a page
--  note that per the MangoPay API the document HAS to be in CREATED status
-- should we check it here? Since MangoPay returns a 500 Internal Server Error if the document is in another status...
createPage :: (MPUsableMonad m) => AnyUserId -> DocumentId -> BS.ByteString -> AccessToken -> MangoPayT m ()
createPage uid did contents at=do
  let val=object ["File" .= TE.decodeUtf8 (B64.encode contents)]
  url<-getClientURLMultiple ["/users/",uid,"/KYC/documents/",did,"/pages"]
  postNoReply url (Just at) val


-- | List all documents uploaded for a user.
--   Since http://docs.mangopay.com/release-hamster/
listDocuments :: (MPUsableMonad m) => AnyUserId -> DocumentFilter -> GenericSort -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Document)
listDocuments uid df gs= genericListExtra (documentFilterAttributes df ++ sortAttributes gs)
  ["/users/",uid,"/KYC/documents"]


-- | List all documents uploaded.
--   Since http://docs.mangopay.com/release-hamster/
listAllDocuments :: (MPUsableMonad m) => DocumentFilter -> GenericSort -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Document)
listAllDocuments df gs = genericListExtra (documentFilterAttributes df ++ sortAttributes gs)
  ["/KYC/documents"]

-- | Id of a document
type DocumentId = Text

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
        parseJSON = jsonRead "DocumentType"

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
        parseJSON = jsonRead "DocumentStatus"

-- | a document
data Document = Document {
  dId                    :: Maybe DocumentId
  ,dCreationDate         :: Maybe POSIXTime
  ,dTag                  :: Maybe Text -- ^  custom data for client
  ,dType                 :: DocumentType
  ,dStatus               :: Maybe DocumentStatus
  ,dRefusedReasonType    :: Maybe Text
  ,dRefusedReasonMessage :: Maybe Text
  } deriving (Show,Ord,Eq,Typeable)


-- | to json as per MangoPay format
instance ToJSON Document where
        toJSON d=objectSN ["Tag" .= dTag d,
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


-- | Do we have a validated document of the given type?
hasValidatedDocument
  :: DocumentType
     -- ^ The document type.
  -> [Document]
     -- ^ The documents we know about.
  -> Bool
hasValidatedDocument dtype = any (\d -> dtype == dType d && Just VALIDATED == dStatus d)


-- | Calculate the MangoPay authentication level.
-- <http://docs.mangopay.com/api-references/kyc-rules/>
getKindOfAuthentication
  :: Either NaturalUser LegalUser
  -> [Document]
  -> KindOfAuthentication
getKindOfAuthentication _ [] = Light
getKindOfAuthentication (Left nu) docs =
  case (uAddress nu,uOccupation nu,uIncomeRange nu,hasValidatedDocument IDENTITY_PROOF docs) of
    (Just _,Just _, Just _,True) -> if hasValidatedDocument ADDRESS_PROOF docs
      then Strong
      else Regular
    _ -> Light
getKindOfAuthentication (Right lu) docs=
  case (lHeadquartersAddress lu,lLegalRepresentativeEmail lu,lLegalRepresentativeAddress lu,
    hasValidatedDocument ARTICLES_OF_ASSOCIATION docs,
    hasValidatedDocument REGISTRATION_PROOF docs,
    hasValidatedDocument SHAREHOLDER_DECLARATION docs) of
    (Just _, Just _, Just _, True, True, True) -> Regular
    _ -> Light


-- | Get the document types that may be required from the given user to enhance authorization level.
getRequiredDocumentTypes
  :: Either NaturalUser LegalUser
     -- ^ The MangoPay user.
  -> [DocumentType]
getRequiredDocumentTypes (Left _) = [IDENTITY_PROOF, ADDRESS_PROOF]
getRequiredDocumentTypes (Right _) = [ARTICLES_OF_ASSOCIATION, REGISTRATION_PROOF, SHAREHOLDER_DECLARATION]


-- | A filter for document lists.
data DocumentFilter = DocumentFilter
  {
    dfBefore :: Maybe POSIXTime
  , dfAfter  :: Maybe POSIXTime
  , dfStatus :: Maybe DocumentStatus
  , dfType   :: Maybe DocumentType
  } deriving (Show,Eq,Ord,Typeable)

instance Default DocumentFilter where
  def = DocumentFilter Nothing Nothing Nothing Nothing

-- | get filter attributes for transaction query
documentFilterAttributes :: DocumentFilter -> [(ByteString,Maybe ByteString)]
documentFilterAttributes f=[ "BeforeDate" ?+ dfBefore f
                                     , "AfterDate" ?+ dfAfter f
                                     , "Status" ?+ (show <$> (dfStatus f))
                                     , "Type" ?+ (show <$> (dfType f))]
