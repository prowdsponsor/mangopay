{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleContexts, ConstraintKinds #-}
-- | refunds on payins and transfers
module Web.MangoPay.Refunds where

import Web.MangoPay.Monad
import Web.MangoPay.Payins
import Web.MangoPay.Types
import Web.MangoPay.Users
import Web.MangoPay.Wallets

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT

-- | refund a transfer
refundTransfer ::  (MPUsableMonad m) => TransferID -> AnyUserID -> AccessToken -> MangoPayT m Refund
refundTransfer tid authID at= do
    url<-getClientURLMultiple ["/transfers/",tid,"/refunds"]
    postExchange url (Just at) (RefundRequest authID Nothing Nothing)

-- | refund a pay-in
refundPayin ::  (MPUsableMonad m) => AnyPayinID -> RefundRequest -> AccessToken -> MangoPayT m Refund
refundPayin pid rr at= do
    url<-getClientURLMultiple ["/payins/",pid,"/refunds"]
    postExchange url (Just at) rr

-- | fetch a refund from its ID
fetchRefund :: (MPUsableMonad m) => RefundID -> AccessToken -> MangoPayT m Refund
fetchRefund rid at=do
        url<-getClientURLMultiple ["/refunds/",rid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req

-- | refund request
data RefundRequest=RefundRequest{
  rrAuthorId :: AnyUserID -- ^ The user ID of the author
  ,rrDebitedFunds :: Maybe Amount -- ^ Strictly positive amount. In cents.
  ,rrFees :: Maybe Amount -- ^ In cents
  }deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON RefundRequest  where
    toJSON rr=object ["AuthorId" .= rrAuthorId rr,"DebitedFunds" .= rrDebitedFunds rr,
      "Fees" .= rrFees rr]


-- | id of a refund
type RefundID = Text

-- | refund of a transfer
data Refund=Refund{
  rId :: RefundID -- ^ Id of the refund
  ,rCreationDate :: POSIXTime
  ,rTag :: Maybe Text -- ^ Custom data
  ,rAuthorId :: AnyUserID -- ^ The user ID of the author
  ,rDebitedFunds :: Amount -- ^ Strictly positive amount. In cents.
  ,rFees :: Amount -- ^ In cents
  ,rCreditedFunds :: Amount -- ^ In cents
  ,rStatus  :: TransferStatus
  ,rResultCode  :: Text -- ^ The transaction result code
  ,rResultMessage :: Maybe Text -- ^ The transaction result Message
  ,rExecutionDate :: POSIXTime
  ,rType :: TransactionType
  ,rNature :: TransactionNature
  ,rCreditedUserId :: Maybe AnyUserID -- ^ Id of the user owner of the credited wallet
  ,rInitialTransactionId  :: TransactionID -- ^ Id of the transaction being refunded
  ,rInitialTransactionType  :: TransactionType -- ^  The type of the transaction before being refunded (PayIn, Refund)
  ,rDebitedWalletId :: WalletID -- ^ The Id of the debited Wallet
  ,rCreditedWalletID  :: Maybe WalletID -- ^ The Id of the credited Wallet
  } deriving (Show,Eq,Ord,Typeable)

-- | from json as per MangoPay format
instance FromJSON Refund where
        parseJSON (Object v) =Refund <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "AuthorId" <*>
                         v .: "DebitedFunds" <*>
                         v .: "Fees" <*>
                         v .: "CreditedFunds" <*>
                         v .: "Status" <*>
                         v .: "ResultCode" <*>
                         v .:? "ResultMessage" <*>
                         v .: "ExecutionDate" <*>
                         v .: "Type" <*>
                         v .: "Nature" <*>
                         v .:? "CreditedUserId" <*>
                         v .: "InitialTransactionId" <*>
                         v .: "InitialTransactionType" <*>
                         v .: "DebitedWalletId" <*>
                         v .:? "CreditedWalletID"
        parseJSON _=fail "Refund"
