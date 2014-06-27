{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
-- | handle payouts
module Web.MangoPay.Payouts where

import Web.MangoPay.Accounts
import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users
import Web.MangoPay.Wallets

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT

-- | create a payout
storePayout ::  (MPUsableMonad m) => Payout -> AccessToken -> MangoPayT m Payout
storePayout pt at= do
    url<-getClientURL "/payouts/bankwire"
    postExchange url (Just at) pt

-- | fetch an payout from its ID
fetchPayout :: (MPUsableMonad m) => PayoutID -> AccessToken -> MangoPayT m Payout
fetchPayout ptid at=do
        url<-getClientURLMultiple ["/payouts/",ptid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req

-- | make a simplep payout for creation
mkPayout :: AnyUserID -> WalletID -> Amount -> Amount -> BankAccountID -> Payout
mkPayout aid wid fds fees bid=Payout Nothing Nothing Nothing aid wid fds fees bid Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing

-- | id of payout
type PayoutID = Text

-- | payout
data Payout=Payout {
  ptId :: Maybe PayoutID
  ,ptCreationDate :: Maybe POSIXTime
  ,ptTag :: Maybe Text -- ^ custom data for client
  ,ptAuthorId :: AnyUserID -- ^ The user ID of the author
  ,ptDebitedWalletId :: WalletID
  ,ptDebitedFunds :: Amount
  ,ptFees :: Amount
  ,ptBankAccountId :: BankAccountID
  ,ptCreditedUserId :: Maybe AnyUserID
  ,ptCreditedFunds :: Maybe Amount
  ,ptStatus :: Maybe TransferStatus
  ,ptResultCode  :: Maybe Text -- ^ The transaction result code
  ,ptResultMessage :: Maybe Text -- ^ The transaction result code
  ,ptExecutionDate :: Maybe  POSIXTime
  ,ptType :: Maybe TransactionType
  ,ptNature :: Maybe TransactionNature
  ,ptPaymentType :: Maybe PaymentType
  ,ptMeanOfPaymentType :: Maybe PaymentType -- ^  « BANK_WIRE »,
  } deriving (Show,Eq,Ord,Typeable)


-- | to json as per MangoPay format
instance ToJSON Payout where
        toJSON pt=object ["Tag" .= ptTag pt,"AuthorId" .= ptAuthorId  pt
          ,"DebitedWalletId" .= ptDebitedWalletId pt
          ,"DebitedFunds" .= ptDebitedFunds pt,"Fees" .= ptFees pt,"BankAccountId" .= ptBankAccountId pt]

-- | from json as per MangoPay format
instance FromJSON Payout where
        parseJSON (Object v) =Payout <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "AuthorId" <*>
                         v .: "DebitedWalletId" <*>
                         v .: "DebitedFunds" <*>
                         v .: "Fees"  <*>
                         v .: "BankAccountId"  <*>
                         v .:? "CreditedUserId"  <*>
                         v .:? "CreditedFunds"  <*>
                         v .:? "Status" <*>
                         v .:? "ResultCode" <*>
                         v .:? "ResultMessage" <*>
                         v .:? "ExecutionDate" <*>
                         v .:? "Type" <*>
                         v .:? "Nature" <*>
                         v .:? "PaymentType" <*>
                         v .:? "MeanOfPaymentType"
        parseJSON _=fail "Payout"


