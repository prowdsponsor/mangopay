{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             FlexibleInstances, OverloadedStrings, PatternGuards,
             ScopedTypeVariables #-}
-- | handle bank accounts
module Web.MangoPay.Accounts where

import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.CountryCodes as C

-- | create an account
createAccount ::  (MPUsableMonad m) => BankAccount -> AccessToken -> MangoPayT m BankAccount
createAccount ba = createGeneric path ba
        where path = BS.concat ["/users/",encodeUtf8 uid,"/bankaccounts/",encodeUtf8 $ typeName $ baDetails ba]
              uid = fromMaybe (error "no user provided for account") $ baUserId ba

-- | fetch an account from its Id
fetchAccount :: (MPUsableMonad m) => AnyUserId -> BankAccountId -> AccessToken -> MangoPayT m BankAccount
fetchAccount uid = fetchGeneric path
        where path = T.concat ["/users/",uid,"/bankaccounts/"]

-- | list all accounts for a given user
listAccounts :: (MPUsableMonad m) => AnyUserId ->  GenericSort -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList BankAccount)
listAccounts uid gs = genericListExtra (sortAttributes gs) ["/users/",uid,"/bankaccounts/"]

-- | account details, depending on the type
data BankAccountDetails=IBAN {
  atIBAN :: Text
  ,atBIC :: Maybe Text
  } | GB {
  atAccountNumber :: Text
  ,atSortCode     :: Text
  } | US {
  atAccountNumber :: Text
  ,atABA          :: Text
  } | CA {
  atAccountNumber      :: Text
  ,atBankName          :: Text
  ,atInstitutionNumber :: Text
  ,atBranchCode        :: Text
  } | Other {
  atAccountNumber :: Text
  ,atBIC          :: Maybe Text
  ,atCountry      :: C.CountryCode
  } deriving (Show,Read,Eq,Ord,Typeable)

-- | from json as per MangoPay format
instance FromJSON BankAccountDetails where
  parseJSON (Object v) =do
    typ<-v .: "Type"
    case typ of
      "IBAN"->IBAN <$>
                v .:  "IBAN" <*>
                v .:? "BIC"
      "GB"->GB <$>
                v .: "AccountNumber" <*>
                v .: "SortCode"
      "US"->US <$>
                v .: "AccountNumber" <*>
                v .: "ABA"
      "CA"->CA <$>
                v .: "AccountNumber" <*>
                v .: "BankName" <*>
                v .: "InstitutionNumber" <*>
                v .: "BranchCode"
      "OTHER"->Other <$>
                v .:  "AccountNumber" <*>
                v .:? "BIC" <*>
                v .:  "Country"
      _->fail $ "BankAccountDetails: unknown type:" ++ typ
  parseJSON _=fail "BankAccountDetails"

-- | type name for details
typeName :: BankAccountDetails -> Text
typeName (IBAN {})="IBAN"
typeName (GB {})="GB"
typeName (US {})="US"
typeName (CA {})="CA"
typeName (Other {})="OTHER"

-- | the details attribute to be added to the account object
toJSONPairs :: BankAccountDetails -> [Pair]
toJSONPairs (IBAN iban bic)=["IBAN" .= iban,"BIC" .= bic]
toJSONPairs (GB nb sc)=["AccountNumber" .= nb,"SortCode" .= sc]
toJSONPairs (US nb aba)=["AccountNumber" .= nb,"ABA" .= aba]
toJSONPairs (CA nb bn inb bc)=["AccountNumber" .= nb,"BankName" .= bn,"InstitutionNumber" .= inb, "BranchCode" .= bc]
toJSONPairs (Other nb bic c)=["AccountNumber" .= nb,"BIC" .= bic,"Country" .= c]


-- | Get the country for a BankAccount
accountCountry :: BankAccount -> Maybe C.CountryCode
accountCountry ba = case baDetails ba of
  GB{} -> Just C.GB
  US{} -> Just C.US
  CA{} -> Just C.CA
  Other _ _ c -> Just c
  IBAN ib _ -> C.fromMText $ T.take 2 ib


-- | Id of a bank account
type BankAccountId = Text

-- | bank account details
data BankAccount = BankAccount {
  baId            :: Maybe BankAccountId
  ,baCreationDate :: Maybe POSIXTime
  ,baUserId       :: Maybe AnyUserId
  ,baTag          :: Maybe Text
  ,baDetails      :: BankAccountDetails
  ,baOwnerName    :: Text
  ,baOwnerAddress :: Maybe Text
} deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON BankAccount where
        toJSON ba=objectSN $ ["OwnerName" .= baOwnerName ba,"Type" .= typeName (baDetails ba)
           ,"OwnerAddress" .= baOwnerAddress ba, "UserId" .= baUserId ba, "Tag" .= baTag ba]
            ++ toJSONPairs (baDetails ba)

-- | from json as per MangoPay format
instance FromJSON BankAccount where
  parseJSON o@(Object v) =
    BankAccount <$>
       v .:? "Id" <*>
       v .:? "CreationDate" <*>
       v .:? "UserId" <*>
       v .:? "Tag" <*>
       parseJSON o <*>
       v .: "OwnerName" <*>
       v .:? "OwnerAddress"
  parseJSON _=fail "BankAccount"

-- | type of payment
data PaymentType = CARD | BANK_WIRE | AUTOMATIC_DEBIT | DIRECT_DEBIT
   deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON PaymentType where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON PaymentType where
        parseJSON (String s)
          | ((a,_):_)<-reads $ unpack s=pure a
        parseJSON _ =fail "PaymentType"
