{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances #-}
-- | handle wallets
module Web.Mangopay.Wallets where


import Web.Mangopay.Monad
import Web.Mangopay.Types
import Web.Mangopay.Users

import Data.Conduit
import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT
import qualified Data.HashMap.Lazy as HM (delete)

-- | create or edit a wallet
storeWallet ::  (MonadBaseControl IO m, MonadResource m) => Wallet -> AccessToken -> MangopayT m Wallet
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
fetchWallet :: (MonadBaseControl IO m, MonadResource m) => WalletID -> AccessToken -> MangopayT m Wallet
fetchWallet wid at=do
        url<-getClientURLMultiple ["/wallets/",wid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req 

-- | list all wallets for a given user   
listWallets :: (MonadBaseControl IO m, MonadResource m) => AnyUserID -> Maybe Pagination -> AccessToken -> MangopayT m [Wallet]
listWallets uid mp at=do
        url<-getClientURLMultiple ["/users/",uid,"/wallets"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONResponse req 

-- | create a new fund transfer 
createTransfer :: (MonadBaseControl IO m, MonadResource m) => Transfer -> AccessToken -> MangopayT m Transfer
createTransfer t at= do
        url<-getClientURL "/transfers"
        postExchange url (Just at) t      
        
-- | fetch a transfer from its ID
fetchTransfer :: (MonadBaseControl IO m, MonadResource m) => TransferID -> AccessToken -> MangopayT m Transfer
fetchTransfer wid at=do
        url<-getClientURLMultiple ["/transfers/",wid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req         

-- | list transfers for a given wallet 
listTransfers ::  (MonadBaseControl IO m, MonadResource m) =>  WalletID  -> Maybe Pagination -> AccessToken -> MangopayT m [Transfer]
listTransfers wid mp at=do
        url<-getClientURLMultiple ["/wallets/",wid,"/transactions"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONResponse req 

-- | list transfer for a given user
listTransfersForUser ::  (MonadBaseControl IO m, MonadResource m) =>  AnyUserID  -> Maybe Pagination -> AccessToken -> MangopayT m [Transfer]
listTransfersForUser uid mp at=do
        url<-getClientURLMultiple ["/users/",uid,"/transactions"]
        req<-getGetRequest url (Just at) (paginationAttributes mp)
        getJSONResponse req 
        
-- | alias for Currency
type Currency=Text

-- | currency amount 
data Amount=Amount {
        bCurrency :: Currency
        ,bAmount :: Double -- ^ all examples show integer values but let's accept doubles for now
        }
        deriving (Show,Read,Eq,Ord,Typeable)
 
-- | to json as per Mangopay format        
instance ToJSON Amount where
        toJSON b=object ["Currency"  .= bCurrency b,"Amount" .= bAmount b]

-- | from json as per Mangopay format 
instance FromJSON Amount where
        parseJSON (Object v) =Amount <$>
                         v .: "Currency" <*>
                         v .: "Amount" 
        parseJSON _=fail "Amount"

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

-- | to json as per Mangopay format        
instance ToJSON Wallet where
        toJSON w=object ["Tag"  .= wTag w,"Owners" .= wOwners w,"Description" .= wDescription w,"Currency" .= wCurrency w]

-- | from json as per Mangopay format 
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
     
-- | to json as per Mangopay format
instance ToJSON TransferStatus  where
    toJSON Created="CREATED"
    toJSON Succeeded="SUCCEEDED"
    toJSON Failed="FAILED"
 
-- | from json as per Mangopay format
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
        
-- | to json as per Mangopay format
instance ToJSON Transfer  where
    toJSON t=object ["AuthorId" .= tAuthorId t,"CreditedUserId" .= tCreditedUserId t,"DebitedFunds" .= tDebitedFunds t,
        "Fees" .= tFees t,"DebitedWalletID" .= tDebitedWalletID t,"CreditedWalletID" .= tCreditedWalletID t,
        "Tag" .= tTag t]
    
 -- | from json as per Mangopay format 
instance FromJSON Transfer where
        parseJSON (Object v) =Transfer <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .: "Tag" <*>
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
    
