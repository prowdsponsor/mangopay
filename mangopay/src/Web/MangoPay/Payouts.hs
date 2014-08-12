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

-- | create a payout
createPayout ::  (MPUsableMonad m) => Payout -> AccessToken -> MangoPayT m Payout
createPayout = createGeneric "/payouts/bankwire"

-- | fetch an payout from its Id
fetchPayout :: (MPUsableMonad m) => PayoutId -> AccessToken -> MangoPayT m Payout
fetchPayout = fetchGeneric "/payouts/"

-- | make a simplep payout for creation
mkPayout :: AnyUserId -> WalletId -> Amount -> Amount -> BankAccountId -> Payout
mkPayout aid wid fds fees bid=Payout Nothing Nothing Nothing aid wid fds fees bid Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing

-- | id of payout
type PayoutId = Text

-- | payout
data Payout=Payout {
  ptId :: Maybe PayoutId
  ,ptCreationDate :: Maybe POSIXTime
  ,ptTag :: Maybe Text -- ^ custom data for client
  ,ptAuthorId :: AnyUserId -- ^ The user Id of the author
  ,ptDebitedWalletId :: WalletId
  ,ptDebitedFunds :: Amount
  ,ptFees :: Amount
  ,ptBankAccountId :: BankAccountId
  ,ptCreditedUserId :: Maybe AnyUserId
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
