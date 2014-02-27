{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, FlexibleInstances #-}
-- | handle events
-- <http://docs.mangopay.com/api-references/events/>
module Web.MangoPay.Events where

import Web.MangoPay.Monad
import Web.MangoPay.Types

import Data.Conduit
import Data.Text hiding (filter)
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Default
import Control.Applicative
import qualified Network.HTTP.Types as HT
import Data.Maybe (isJust)


-- | create or edit a natural user
searchEvents ::  (MonadBaseControl IO m, MonadResource m) => EventSearchParams -> AccessToken -> MangoPayT m  [Event]
searchEvents esp at=do
        url<-getClientURL "/events"
        req<-getGetRequest url (Just at) esp
        getJSONResponse req

-- | Event type
data EventType=PAYIN_NORMAL_CREATED
        | PAYIN_NORMAL_SUCCEEDED
        | PAYIN_NORMAL_FAILED
        | PAYOUT_NORMAL_CREATED
        | PAYOUT_NORMAL_SUCCEEDED
        | PAYOUT_NORMAL_FAILED
        | TRANSFER_NORMAL_CREATED
        | TRANSFER_NORMAL_SUCCEEDED
        | TRANSFER_NORMAL_FAILED
        | PAYIN_REFUND_CREATED
        | PAYIN_REFUND_SUCCEEDED
        | PAYIN_REFUND_FAILED
        | PAYOUT_REFUND_CREATED
        | PAYOUT_REFUND_SUCCEEDED
        | PAYOUT_REFUND_FAILED
        | TRANSFER_REFUND_CREATED
        | TRANSFER_REFUND_SUCCEEDED
        | TRANSFER_REFUND_FAILED
        deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

instance ToHtQuery (Maybe EventType) where
  n ?+ d=n ?+ fmap show d

-- | to json as per MangoPay format
instance ToJSON EventType where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON EventType where
        parseJSON (String s)=pure $ read $ unpack s
        parseJSON _ =fail "EventType"

-- | search parameters for events        
data EventSearchParams=EventSearchParams{
        espEventType :: Maybe EventType
        ,espBeforeDate :: Maybe POSIXTime
        ,espAfterDate :: Maybe POSIXTime
        ,espPagination :: Maybe Pagination
        }
        deriving (Show,Eq,Ord,Typeable)
 
instance Default EventSearchParams where
  def=EventSearchParams Nothing Nothing Nothing Nothing
  
instance HT.QueryLike EventSearchParams where
  toQuery (EventSearchParams et bd ad p)=filter (isJust .snd) 
    ["eventtype" ?+ et
    ,"beforeDate" ?+ bd
    ,"afterDate" ?+ ad
   ] ++ paginationAttributes p 
        
--instance ToJSON EventSearchParams where
--        toJSON esp=object $ ["eventtype" .= espEventType esp,
--                "beforeDate" .= espBeforeDate esp, "afterDate" .= espAfterDate esp]
--                ++ paginationAttributes (espPagination esp)

-- | a event
data Event=Event {
        eResourceId :: Text
        ,eEventType :: EventType
        ,eDate :: POSIXTime
        }
        deriving (Show,Eq,Ord,Typeable)
 
-- | to json as per MangoPay format        
instance ToJSON Event where
        toJSON e=object ["RessourceID"  .= eResourceId e,"EventType" .= eEventType e,"Date" .= eDate e]

-- | from json as per MangoPay format 
instance FromJSON Event where
        parseJSON (Object v) =Event <$>
                         v .: "RessourceID" <*>
                         v .: "EventType" <*>
                         v .: "Date"
        parseJSON _=fail "Event"
                       