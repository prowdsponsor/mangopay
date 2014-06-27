{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances, PatternGuards, ConstraintKinds #-}
-- | handle wallets
module Web.MangoPay.Wallets where


import Web.MangoPay.Monad
import Web.MangoPay.Types
import Web.MangoPay.Users

import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT
import qualified Data.HashMap.Lazy as HM (delete)

-- | create or edit a wallet
storeWallet ::  (MPUsableMonad m) => Wallet -> AccessToken -> MangoPayT m Wallet
storeWallet w at=
        case wId w of
                Nothing-> do
                        url<-getClientURL "/wallets"
                        postExchange url (Just at) w
                Just i-> do
                        url<-getClientURLMultiple ["/wallets/",i]
                        let Object m=toJSON w
                        putExchange url (Just at) (Object $ HM.delete "Currency" m)


-- | fetch a wallet from its ID
fetchWallet :: (MPUsableMonad m) => WalletID -> AccessToken -> MangoPayT m Wallet
fetchWallet wid at=do
        url<-getClientURLMultiple ["/wallets/",wid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req

-- | list all wallets for a given user
listWallets :: (MPUsableMonad m) => AnyUserID -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Wallet)
listWallets uid mp at=do
        url<-getClientURLMultiple ["/users/",uid,"/wallets"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONList req

-- | create a new fund transfer
createTransfer :: (MPUsableMonad m) => Transfer -> AccessToken -> MangoPayT m Transfer
createTransfer t at= do
        url<-getClientURL "/transfers"
        postExchange url (Just at) t

-- | fetch a transfer from its ID
fetchTransfer :: (MPUsableMonad m) => TransferID -> AccessToken -> MangoPayT m Transfer
fetchTransfer wid at=do
        url<-getClientURLMultiple ["/transfers/",wid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req

-- | list transfers for a given wallet
listTransactions ::  (MPUsableMonad m) =>  WalletID  -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Transaction)
listTransactions wid mp at=do
        url<-getClientURLMultiple ["/wallets/",wid,"/transactions"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONList req

-- | list transfer for a given user
listTransactionsForUser ::  (MPUsableMonad m) =>  AnyUserID  -> Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Transaction)
listTransactionsForUser uid mp at=do
        url<-getClientURLMultiple ["/users/",uid,"/transactions"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONList req


-- | ID of a wallet
type WalletID=Text

-- | a wallet
data Wallet = Wallet {
        wId:: Maybe WalletID -- ^ The Id of the wallet
        ,wCreationDate :: Maybe POSIXTime -- ^ The creation date of the object
        ,wTag :: Maybe Text -- ^  Custom data
        ,wOwners :: [Text] -- ^ The owner of the wallet
        ,wDescription :: Text -- ^ A description of the wallet
        ,wCurrency :: Currency -- ^ Currency of the wallet
        ,wBalance :: Maybe Amount -- ^ The amount held on the wallet
        }
        deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Wallet where
        toJSON w=object ["Tag"  .= wTag w,"Owners" .= wOwners w,"Description" .= wDescription w,"Currency" .= wCurrency w]

-- | from json as per MangoPay format
instance FromJSON Wallet where
        parseJSON (Object v) =Wallet <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "Owners" <*>
                         v .: "Description" <*>
                         v .: "Currency" <*>
                         v .: "Balance"
        parseJSON _=fail "Wallet"


-- | ID of a transfer
type TransferID=Text

-- | status of a transfer
data TransferStatus= Created | Succeeded | Failed
     deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON TransferStatus  where
    toJSON Created="CREATED"
    toJSON Succeeded="SUCCEEDED"
    toJSON Failed="FAILED"

-- | from json as per MangoPay format
instance FromJSON TransferStatus where
    parseJSON (String "CREATED") =pure Created
    parseJSON (String "SUCCEEDED") =pure Succeeded
    parseJSON (String "FAILED") =pure Failed
    parseJSON _= fail "TransferStatus"

-- | transfer between wallets
data Transfer = Transfer{
        tId :: Maybe TransferID -- ^ Id of the transfer
        ,tCreationDate    :: Maybe POSIXTime -- ^  The creation date of the object
        ,tTag     :: Maybe Text -- ^   Custom data
        ,tAuthorId :: AnyUserID -- ^ The Id of the author
        ,tCreditedUserId  :: Maybe AnyUserID -- ^ The Id of the user owner of the credited wallet
        ,tDebitedFunds :: Amount -- ^ The funds debited from the « debited wallet »DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,tFees  :: Amount -- ^  The fees taken on the transfer.DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,tDebitedWalletID :: WalletID -- ^  The debited wallet (where the funds are held before the transfer)
        ,tCreditedWalletID:: WalletID -- ^ The credited wallet (where the funds will be held after the transfer)
        ,tCreditedFunds :: Maybe Amount -- ^  The funds credited on the « credited wallet »DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,tStatus  :: Maybe TransferStatus -- ^   The status of the transfer:
        ,tResultCode      :: Maybe Text -- ^   The transaction result code
        ,tResultMessage   :: Maybe Text -- ^   The transaction result message
        ,tExecutionDate   :: Maybe POSIXTime -- ^  The execution date of the transfer
        }
        deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Transfer  where
    toJSON t=object ["AuthorId" .= tAuthorId t,"CreditedUserId" .= tCreditedUserId t,"DebitedFunds" .= tDebitedFunds t,
        "Fees" .= tFees t,"DebitedWalletID" .= tDebitedWalletID t,"CreditedWalletID" .= tCreditedWalletID t,
        "Tag" .= tTag t]

-- | from json as per MangoPay format
instance FromJSON Transfer where
        parseJSON (Object v) =Transfer <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "AuthorId" <*>
                         v .: "CreditedUserId" <*>
                         v .: "DebitedFunds" <*>
                         v .: "Fees" <*>
                         v .: "DebitedWalletId" <*> -- yes, it's ID one way, Id the other
                         v .: "CreditedWalletId" <*> -- yes, it's ID one way, Id the other
                         v .:? "CreditedFunds" <*>
                         v .:? "Status" <*>
                         v .:? "ResultCode" <*>
                         v .:? "ResultMessage" <*>
                         v .:? "ExecutionDate"
        parseJSON _=fail "Transfer"

-- | type of transaction
data TransactionType = PAYIN
  | PAYOUT
  | TRANSFER
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON TransactionType where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON TransactionType where
        parseJSON (String s)
          | ((a,_):_)<-reads $ unpack s=pure a
        parseJSON _ =fail "TransactionType"

data TransactionNature =  REGULAR -- ^ just as you created the object
 | REFUND -- ^ the transaction has been refunded
 | REPUDIATION -- ^ the transaction has been repudiated
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON TransactionNature where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON TransactionNature where
        parseJSON (String s)
          | ((a,_):_)<-reads $ unpack s=pure a
        parseJSON _ =fail "TransactionNature"


type TransactionID = Text

-- | any transaction
data Transaction = Transaction{
        txId :: Maybe TransactionID -- ^ Id of the transfer
        ,txCreationDate    :: Maybe POSIXTime -- ^  The creation date of the object
        ,txTag     :: Maybe Text -- ^   Custom data
        ,txAuthorId :: AnyUserID -- ^ The Id of the author
        ,txCreditedUserId  :: Maybe AnyUserID -- ^ The Id of the user owner of the credited wallet
        ,txDebitedFunds :: Amount -- ^ The funds debited from the « debited wallet »DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,txFees  :: Amount -- ^  The fees taken on the transfer.DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,txDebitedWalletID :: Maybe WalletID -- ^  The debited wallet (where the funds are held before the transfer)
        ,txCreditedWalletID:: Maybe WalletID -- ^ The credited wallet (where the funds will be held after the transfer)
        ,txCreditedFunds :: Maybe Amount -- ^  The funds credited on the « credited wallet »DebitedFunds – Fees = CreditedFunds (amount received on wallet)
        ,txStatus  :: Maybe TransferStatus -- ^   The status of the transfer:
        ,txResultCode      :: Maybe Text -- ^   The transaction result code
        ,txResultMessage   :: Maybe Text -- ^   The transaction result message
        ,txExecutionDate   :: Maybe POSIXTime -- ^  The execution date of the transfer
        ,txType  :: TransactionType -- ^  The type of the transaction
        ,txNature  :: TransactionNature -- ^  The nature of the transaction:
        }
        deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Transaction  where
    toJSON t=object ["AuthorId" .= txAuthorId t,"CreditedUserId" .= txCreditedUserId t,"DebitedFunds" .= txDebitedFunds t,
        "Fees" .= txFees t,"DebitedWalletID" .= txDebitedWalletID t,"CreditedWalletID" .= txCreditedWalletID t,
        "Tag" .= txTag t,"Type" .= txType t,"Nature" .= txNature t]

-- | from json as per MangoPay format
instance FromJSON Transaction where
        parseJSON (Object v) =Transaction <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .:? "Tag" <*>
                         v .: "AuthorId" <*>
                         v .: "CreditedUserId" <*>
                         v .: "DebitedFunds" <*>
                         v .: "Fees" <*>
                         v .:? "DebitedWalletId" <*> -- yes, it's ID one way, Id the other
                         v .:? "CreditedWalletId" <*> -- yes, it's ID one way, Id the other
                         v .:? "CreditedFunds" <*>
                         v .:? "Status" <*>
                         v .:? "ResultCode" <*>
                         v .:? "ResultMessage" <*>
                         v .:? "ExecutionDate" <*>
                         v .: "Type"  <*>
                         v .: "Nature"
        parseJSON _=fail "Transfer"
