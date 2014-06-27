{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances, PatternGuards, ConstraintKinds #-}
-- | handle bank accounts
module Web.MangoPay.Accounts where

import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT

import Data.CountryCodes (CountryCode)

-- | create an account
storeAccount ::  (MPUsableMonad m) => BankAccount -> AccessToken -> MangoPayT m BankAccount
storeAccount ba at
  | Just uid<-baUserId ba= do
    url<-getClientURLMultiple ["/users/",uid,"/bankaccounts/",typeName $ baDetails ba]
    postExchange url (Just at) ba
  | otherwise=error "no user provided for account"

-- | fetch an account from its ID
fetchAccount :: (MPUsableMonad m) => AnyUserID -> BankAccountID -> AccessToken -> MangoPayT m BankAccount
fetchAccount uid aid at=do
        url<-getClientURLMultiple ["/users/",uid,"/bankaccounts/",aid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req

-- | list all accounts for a given user
listAccounts :: (MPUsableMonad m) => AnyUserID -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList BankAccount)
listAccounts uid mp at=do
        url<-getClientURLMultiple ["/users/",uid,"/bankaccounts/"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONList req

-- | account details, depending on the type
data BankAccountDetails=IBAN {
  atIBAN :: Text
  ,atBIC :: Text
  } | GB {
  atAccountNumber :: Text
  ,atSortCode :: Text
  } | US {
  atAccountNumber :: Text
  ,atABA :: Text
  } | CA {
  atAccountNumber :: Text
  ,atBankName :: Text
  ,atInstitutionNumber :: Text
  ,atBranchCode :: Text
  } | Other {
  atAccountNumber :: Text
  ,atBIC :: Text
  ,atCountry :: CountryCode
  } deriving (Show,Read,Eq,Ord,Typeable)

-- | from json as per MangoPay format
instance FromJSON BankAccountDetails where
  parseJSON (Object v) =do
    typ<-v .: "Type"
    case typ of
      "IBAN"->IBAN <$>
                v .: "IBAN" <*>
                v .: "BIC"
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
                v .: "AccountNumber" <*>
                v .: "BIC" <*>
                v .: "Country"
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

-- | ID of a bank account
type BankAccountID = Text

-- | bank account details
data BankAccount = BankAccount {
  baId :: Maybe BankAccountID
  ,baCreationDate :: Maybe POSIXTime
  ,baUserId :: Maybe AnyUserID
  ,baTag :: Maybe Text
  ,baDetails :: BankAccountDetails
  ,baOwnerName :: Text
  ,baOwnerAddress :: Maybe Text
} deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON BankAccount where
        toJSON ba=object $ ["OwnerName" .= baOwnerName ba,"Type" .= typeName (baDetails ba)
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
