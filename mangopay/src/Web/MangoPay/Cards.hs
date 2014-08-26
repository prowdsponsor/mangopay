{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances, ConstraintKinds, RecordWildCards #-}
-- | handle cards
module Web.MangoPay.Cards where

import Web.MangoPay.Documents
import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative

import qualified Data.HashMap.Lazy as HM

-- | card registration Id
type CardRegistrationId=Text


-- | create a card registration
createCardRegistration ::  (MPUsableMonad m) => CardRegistration -> AccessToken -> MangoPayT m CardRegistration
createCardRegistration = createGeneric "/cardregistrations"


-- | modify a card registration
modifyCardRegistration ::  (MPUsableMonad m) => CardRegistration -> AccessToken -> MangoPayT m CardRegistration
modifyCardRegistration cr = modifyGGeneric
    (Just $  HM.filterWithKey (\k _->k=="RegistrationData")) "/cardregistrations/" cr crId

-- | credit card information
data CardInfo = CardInfo {
  ciNumber :: Text
  ,ciExpire :: CardExpiration
  ,ciCSC :: Text
  } deriving (Show,Read,Eq,Ord,Typeable)


-- | helper function to create a new card registration
mkCardRegistration :: AnyUserId -> Currency -> CardRegistration
mkCardRegistration uid currency=CardRegistration Nothing Nothing Nothing uid currency Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | a card registration
data CardRegistration = CardRegistration {
  crId :: Maybe CardRegistrationId -- ^ The Id of the object
  ,crCreationDate  :: Maybe POSIXTime -- ^ The creation date of the object
  ,crTag :: Maybe Text -- ^  Custom data
  ,crUserId  :: AnyUserId -- ^  The Id of the author
  ,crCurrency  :: Currency -- ^ The currency of the card registrated
  ,crAccessKey :: Maybe Text -- ^ This key has to be sent with the card details and the PreregistrationData
  ,crPreregistrationData  :: Maybe Text -- ^  This passphrase has to be sent with the card details and the AccessKey
  ,crCardRegistrationURL  :: Maybe Text -- ^  The URL where to POST the card details, the AccessKey and PreregistrationData
  ,crRegistrationData   :: Maybe Text -- ^  You get the CardRegistrationData once you posted the card details, the AccessKey and PreregistrationData
  ,crCardType   :: Maybe Text -- ^  « CB_VISA_MASTERCARD » is the only value available yet
  ,crCardId   :: Maybe CardId -- ^  You get the CardId (to process payments) once you edited the CardRegistration Object with the RegistrationData
  ,crResultCode   :: Maybe Text -- ^  The result code of the object
  ,crResultMessage  :: Maybe Text -- ^  The message explaining the result code
  ,crStatus  :: Maybe DocumentStatus -- ^ The status of the object.
} deriving (Show,Eq,Ord,Typeable)


-- | to json as per MangoPay format
instance ToJSON CardRegistration where
        toJSON cr=object ["Id".= crId cr -- we store the Id, because in the registration workflow we may need to hang on to the registration object for a while, so let's use JSON serialization to keep it!
          , "Tag" .= crTag cr,"UserId" .= crUserId cr
          ,"Currency" .= crCurrency cr,"RegistrationData" .= crRegistrationData cr
          ,"CardRegistrationURL" .= crCardRegistrationURL cr]

-- | from json as per MangoPay format
instance FromJSON CardRegistration where
        parseJSON (Object v) =CardRegistration <$>
                         v .: "Id" <*>
                         v .:? "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "UserId" <*>
                         v .: "Currency" <*>
                         v .:? "AccessKey"  <*>
                         v .:? "PreregistrationData"  <*>
                         v .:? "CardRegistrationURL"  <*>
                         v .:? "RegistrationData"  <*>
                         v .:? "CardType"  <*>
                         v .:? "CardId"  <*>
                         v .:? "ResultCode"  <*>
                         v .:? "ResultMessage"  <*>
                         v .:? "Status"
        parseJSON _=fail "CardRegistration"

-- | fetch a card from its Id
fetchCard :: (MPUsableMonad m) => CardId -> AccessToken -> MangoPayT m Card
fetchCard = fetchGeneric "/cards/"

-- | list all cards for a given user
listCards :: (MPUsableMonad m) => AnyUserId -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Card)
listCards uid = genericList ["/users/",uid,"/cards"]

-- | validity of a card
data CardValidity=UNKNOWN | VALID | INVALID
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)


-- | to json as per MangoPay format
instance ToJSON CardValidity where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON CardValidity where
        parseJSON (String s)=pure $ read $ unpack s
        parseJSON _ =fail "CardValidity"


-- | a registered card
data Card=Card {
  cId :: CardId
  ,cCreationDate :: POSIXTime
  ,cTag :: Maybe Text
  ,cExpirationDate   :: CardExpiration -- ^  MMYY
  ,cAlias :: Text -- ^ Example: 497010XXXXXX4414
  ,cCardProvider  :: Text -- ^ The card provider, it could be « CB », « VISA », « MASTERCARD », etc.
  ,cCardType :: Text -- ^ « CB_VISA_MASTERCARD » is the only value available yet
  ,cProduct :: Maybe Text
  ,cBankCode  :: Maybe Text
  ,cActive :: Bool
  ,cCurrency :: Currency
  ,cValidity :: CardValidity -- ^ Once we proceed (or attempted to process) a payment with the card we are able to indicate if it is « valid » or « invalid ». If we didn’t process a payment yet the « Validity » stay at « unknown ».
  ,cCountry :: Text
  ,cUserId :: AnyUserId
  } deriving (Show,Eq,Ord,Typeable)

-- | from json as per MangoPay format
instance FromJSON Card where
        parseJSON (Object v) =Card <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "ExpirationDate" <*>
                         v .: "Alias" <*>
                         v .: "CardProvider"  <*>
                         v .: "CardType"  <*>
                         v .:? "Product"  <*>
                         v .:? "BankCode"  <*>
                         v .: "Active"  <*>
                         v .: "Currency"  <*>
                         v .: "Validity" <*>
                         v .: "Country" <*>
                         v .: "UserId"
        parseJSON _=fail "Card"

-- | to json as per MangoPay format
instance ToJSON Card where
  toJSON Card {..} = object
    [ "Id"             .= cId
    , "CreationDate"   .= cCreationDate
    , "Tag"            .= cTag
    , "ExpirationDate" .= cExpirationDate
    , "Alias"          .= cAlias
    , "CardProvider"   .= cCardProvider
    , "CardType"       .= cCardType
    , "Product"        .= cProduct
    , "BankCode"       .= cBankCode
    , "Active"         .= cActive
    , "Currency"       .= cCurrency
    , "Validity"       .= cValidity
    , "Country"        .= cCountry
    , "UserId"         .= cUserId ]
