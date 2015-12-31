{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
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
import Control.Applicative

-- | create a payout
createPayout ::  (MPUsableMonad m) => Payout -> AccessToken -> MangoPayT m Payout
createPayout = createGeneric "/payouts/bankwire"

-- | fetch an payout from its Id
fetchPayout :: (MPUsableMonad m) => PayoutId -> AccessToken -> MangoPayT m Payout
fetchPayout = fetchGeneric "/payouts/"

-- | make a simple payout for creation
mkPayout :: AnyUserId -> WalletId -> Amount -> Amount -> BankAccountId -> Payout
mkPayout aid wid fds fees bid=Payout Nothing Nothing Nothing aid wid fds fees bid Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing

-- | id of payout
type PayoutId = Text

-- | payout
data Payout=Payout {
  ptId                 :: Maybe PayoutId
  ,ptCreationDate      :: Maybe MpTime
  ,ptTag               :: Maybe Text -- ^ custom data for client
  ,ptAuthorId          :: AnyUserId -- ^ The user Id of the author
  ,ptDebitedWalletId   :: WalletId
  ,ptDebitedFunds      :: Amount
  ,ptFees              :: Amount
  ,ptBankAccountId     :: BankAccountId -- ^ The ID of the bank account object
  ,ptCreditedUserId    :: Maybe AnyUserId
  ,ptCreditedFunds     :: Maybe Amount
  ,ptStatus            :: Maybe TransferStatus
  ,ptResultCode        :: Maybe Text -- ^ The transaction result code
  ,ptResultMessage     :: Maybe Text -- ^ The transaction result code
  ,ptExecutionDate     :: Maybe  MpTime
  ,ptType              :: Maybe TransactionType
  ,ptNature            :: Maybe TransactionNature
  ,ptPaymentType       :: Maybe PaymentType
  ,ptMeanOfPaymentType :: Maybe PaymentType -- ^  « BANK_WIRE »,
  ,ptBankWireRef       :: Maybe Text -- ^ A custom reference you wish to appear on the user’s bank statement
                                     -- (your ClientId is already shown)
                                     -- since <http://docs.mangopay.com/release-hamster/>
  } deriving (Show,Eq,Ord,Typeable)


-- | to json as per MangoPay format
instance ToJSON Payout where
        toJSON pt=objectSN ["Tag" .= ptTag pt,"AuthorId" .= ptAuthorId  pt
          ,"DebitedWalletId" .= ptDebitedWalletId pt
          ,"DebitedFunds" .= ptDebitedFunds pt,"Fees" .= ptFees pt,"BankAccountId" .= ptBankAccountId pt
          ,"BankWireRef" .= ptBankWireRef pt]

-- | from json as per MangoPay format
instance FromJSON Payout where
        parseJSON (Object v) =Payout <$>
                         v .:? "Id" <*>
                         v .:? "CreationDate" <*>
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
                         v .:? "MeanOfPaymentType" <*>
                         v .:? "BankWireRef"
        parseJSON _=fail "Payout"
