{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             OverloadedStrings, PatternGuards #-}
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
import Control.Applicative

-- | refund a transfer
refundTransfer ::  (MPUsableMonad m) => TransferId -> AnyUserId -> AccessToken -> MangoPayT m Refund
refundTransfer tid authId at= do
    url<-getClientURLMultiple ["/transfers/",tid,"/refunds"]
    postExchange url (Just at) (RefundRequest authId Nothing Nothing)

-- | refund a pay-in
refundPayin ::  (MPUsableMonad m) => AnyPayinId -> RefundRequest -> AccessToken -> MangoPayT m Refund
refundPayin pid rr at= do
    url<-getClientURLMultiple ["/payins/",pid,"/refunds"]
    postExchange url (Just at) rr


-- | fetch a refund from its Id
fetchRefund :: (MPUsableMonad m) => RefundId -> AccessToken -> MangoPayT m Refund
fetchRefund = fetchGeneric "/refunds/"

-- | refund request
data RefundRequest=RefundRequest{
  rrAuthorId      :: AnyUserId -- ^ The user Id of the author
  ,rrDebitedFunds :: Maybe Amount -- ^ Strictly positive amount. In cents.
  ,rrFees         :: Maybe Amount -- ^ In cents
  }deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON RefundRequest  where
    toJSON rr=objectSN ["AuthorId" .= rrAuthorId rr,"DebitedFunds" .= rrDebitedFunds rr,
      "Fees" .= rrFees rr]


-- | id of a refund
type RefundId = Text

-- | refund of a transfer
data Refund=Refund{
  rId                      :: RefundId -- ^ Id of the refund
  ,rCreationDate           :: MpTime
  ,rTag                    :: Maybe Text -- ^ Custom data
  ,rAuthorId               :: AnyUserId -- ^ The user Id of the author
  ,rDebitedFunds           :: Amount -- ^ Strictly positive amount. In cents.
  ,rFees                   :: Amount -- ^ In cents
  ,rCreditedFunds          :: Amount -- ^ In cents
  ,rStatus                 :: TransferStatus
  ,rResultCode             :: Text -- ^ The transaction result code
  ,rResultMessage          :: Maybe Text -- ^ The transaction result Message
  ,rExecutionDate          :: Maybe MpTime
  ,rType                   :: TransactionType
  ,rNature                 :: TransactionNature
  ,rCreditedUserId         :: Maybe AnyUserId -- ^ Id of the user owner of the credited wallet
  ,rInitialTransactionId   :: TransactionId -- ^ Id of the transaction being refunded
  ,rInitialTransactionType :: TransactionType -- ^  The type of the transaction before being refunded (PayIn, Refund)
  ,rDebitedWalletId        :: WalletId -- ^ The Id of the debited Wallet
  ,rCreditedWalletId       :: Maybe WalletId -- ^ The Id of the credited Wallet
  ,rReason                 :: RefundReason -- ^ The reason from the refund, since <http://docs.mangopay.com/release-lapin/>
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
                         v .:? "ExecutionDate" <*>
                         v .: "Type" <*>
                         v .: "Nature" <*>
                         v .:? "CreditedUserId" <*>
                         v .: "InitialTransactionId" <*>
                         v .: "InitialTransactionType" <*>
                         v .: "DebitedWalletId" <*>
                         v .:? "CreditedWalletID" <*>
                         v .: "RefundReason"
        parseJSON _=fail "Refund"


-- | Type for redund reason, since <http://docs.mangopay.com/release-lapin/>.
data RefundReasonType =
    BANKACCOUNT_HAS_BEEN_CLOSED
  | INITIALIZED_BY_CLIENT
  | OTHER
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)


-- | to json as per MangoPay format
instance ToJSON RefundReasonType where
        toJSON =toJSON . show


-- | from json as per MangoPay format
instance FromJSON RefundReasonType where
        parseJSON = jsonRead "RefundReasonType"


-- | Reason for a refund, since <http://docs.mangopay.com/release-lapin/>.
data RefundReason = RefundReason
  { rrMessage :: Maybe Text
  , rrType    :: RefundReasonType
  } deriving (Show,Eq,Ord,Typeable)


-- | from json as per MangoPay format
instance FromJSON RefundReason where
        parseJSON (Object v) =RefundReason <$>
                         v .:? "RefundReasonMessage" <*>
                         v .: "RefundReasonType"
        parseJSON _=fail "RefundReason"
