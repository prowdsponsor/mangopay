{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
-- | handle events
-- <http://docs.mangopay.com/api-references/events/>
module Web.MangoPay.Events where

import Web.MangoPay.Monad
import Web.MangoPay.Types

import Data.Text hiding (filter,map,toLower)
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Default
import Control.Applicative
import qualified Network.HTTP.Types as HT
import Data.Maybe (isJust)
import qualified Data.HashMap.Lazy as HM (delete)
import qualified Data.Text.Encoding as TE
import Control.Monad (join)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)

-- | search events, returns a paginated list
searchEvents ::  (MPUsableMonad m) => EventSearchParams -> AccessToken -> MangoPayT m  (PagedList Event)
searchEvents esp at=do
        url<-getClientURL "/events"
        req<-getGetRequest url (Just at) esp
        getJSONList req


-- | search events, returns the full result
searchAllEvents ::  (MPUsableMonad m) => EventSearchParams -> AccessToken -> MangoPayT m  [Event]
searchAllEvents esp at=getAll (\p -> searchEvents esp{espPagination=p}) at


-- | Check if an event came from MangoPay.  Since notifications
-- are not authenticated, you're advised to always check if the
-- event really comes from MangoPay (cf.
-- <https://mangopay.desk.com/customer/portal/questions/6493147>).
checkEvent :: (MPUsableMonad m) => Event -> AccessToken ->  MangoPayT m Bool
checkEvent evt at= do
  -- documentation doesn't say it, but tests show dates are inclusive
  let dt = Just $ eDate evt
  let esp = EventSearchParams (Just $ eEventType evt) dt dt Nothing Nothing
  evts <- searchAllEvents esp at
  return $ evt `elem` evts


-- | create a hook
createHook ::  (MPUsableMonad m) => Hook -> AccessToken -> MangoPayT m Hook
createHook = createGeneric "/hooks"


-- | modify a hook
modifyHook ::  (MPUsableMonad m) => Hook -> AccessToken -> MangoPayT m Hook
modifyHook h = modifyGGeneric (Just $ HM.delete "EventType") "/hooks/" h hId

-- | fetch a wallet from its Id
fetchHook :: (MPUsableMonad m) => HookId -> AccessToken -> MangoPayT m Hook
fetchHook = fetchGeneric "/hooks/"

-- | list all wallets for a given user
listHooks :: (MPUsableMonad m) =>  Maybe Pagination -> AccessToken -> MangoPayT m (PagedList Hook)
listHooks = genericList ["/hooks"]

-- | Event type
data EventType=
          PAYIN_NORMAL_CREATED
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
        | KYC_CREATED
        | KYC_VALIDATION_ASKED
        | KYC_SUCCEEDED
        | KYC_FAILED
        deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

instance ToHtQuery (Maybe EventType) where
  n ?+ d=n ?+ fmap show d

-- | to json as per MangoPay format
instance ToJSON EventType where
        toJSON =toJSON . show

-- | from json as per MangoPay format
instance FromJSON EventType where
        parseJSON = jsonRead "EventType"

-- | search parameters for events
data EventSearchParams=EventSearchParams{
        espEventType   :: Maybe EventType
        ,espBeforeDate :: Maybe MpTime
        ,espAfterDate  :: Maybe MpTime
        ,espPagination :: Maybe Pagination
        ,espSortByDate :: Maybe SortDirection
        }
        deriving (Show,Eq,Ord,Typeable)

instance Default EventSearchParams where
  def=EventSearchParams Nothing Nothing Nothing Nothing Nothing

instance HT.QueryLike EventSearchParams where
  toQuery (EventSearchParams et bd ad p ms)=filter (isJust .snd)
    ["eventtype" ?+ et
    ,"beforeDate" ?+ bd
    ,"afterDate" ?+ ad
    ,"Sort" ?+ ((("Date:" ++) . map toLower . show) <$> ms)
   ] ++ paginationAttributes p

--instance ToJSON EventSearchParams where
--        toJSON esp=object $ ["eventtype" .= espEventType esp,
--                "beforeDate" .= espBeforeDate esp, "afterDate" .= espAfterDate esp]
--                ++ paginationAttributes (espPagination esp)

-- | a event
data Event=Event {
        eResourceId :: Text
        ,eEventType :: EventType
        ,eDate      :: MpTime
        }
        deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Event where
        toJSON e=objectSN ["ResourceId"  .= eResourceId e,"EventType" .= eEventType e,"Date" .= eDate e]

-- | from json as per MangoPay format
instance FromJSON Event where
        parseJSON (Object v) =Event <$>
                         v .: "ResourceId" <*>
                         v .: "EventType" <*>
                         v .: "Date"
        parseJSON _=fail "Event"

-- | parse an event from the query string
-- the MangoPay is not very clear on notifications, but see v1 <http://docs.mangopay.com/v1-api-references/notifications/>
-- v2 works the same, the event is passed via parameters of the query string
eventFromQueryString :: HT.Query -> Maybe Event
eventFromQueryString q=do
  rid <- fmap TE.decodeUtf8 $ join $ findAssoc q "RessourceId" -- yes, two ss here
  et  <- join $ fmap (maybeRead . BS.unpack) $ join $ findAssoc q "EventType"
  d   <- fmap (MpTime . fromIntegral) $ join $ fmap ((maybeRead :: String -> Maybe Integer). BS.unpack) $ join $ findAssoc q "Date"
  return $ Event rid et d


-- | parse an event from the query string represented as Text
-- the MangoPay is not very clear on notifications, but see v1 <http://docs.mangopay.com/v1-api-references/notifications/>
-- v2 works the same, the event is passed via parameters of the query string
eventFromQueryStringT :: [(Text, Text)] -> Maybe Event
eventFromQueryStringT q=do
  rid <- findAssoc q "RessourceId" -- yes, two ss here
  et  <- join $ fmap (maybeRead . unpack) $ findAssoc q "EventType"
  d   <- fmap (MpTime . fromIntegral) $ join $ fmap ((maybeRead :: String -> Maybe Integer). unpack) $ findAssoc q "Date"
  return $ Event rid et d

-- | status of notification hook
data HookStatus=Enabled | Disabled
       deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON HookStatus  where
    toJSON Enabled="ENABLED"
    toJSON Disabled="DISABLED"

-- | from json as per MangoPay format
instance FromJSON HookStatus where
    parseJSON (String "ENABLED") =pure Enabled
    parseJSON (String "DISABLED") =pure Disabled
    parseJSON _= fail "HookStatus"

-- | validity of notification hook
data HookValidity=Valid | Invalid
       deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per MangoPay format
instance ToJSON HookValidity  where
    toJSON Valid="VALID"
    toJSON Invalid="INVALID"

-- | from json as per MangoPay format
instance FromJSON HookValidity where
    parseJSON (String "VALID") =pure Valid
    parseJSON (String "INVALID") =pure Invalid
    parseJSON _= fail "HookValidity"

-- | id for hook
type HookId=Text

-- | a notification hook
data Hook=Hook {
        hId            :: Maybe HookId -- ^ The Id of the hook details
        ,hCreationDate :: Maybe MpTime
        ,hTag          :: Maybe Text -- ^ Custom data
        ,hUrl          :: Text -- ^This is the URL where you receive notification for each EventType
        ,hStatus       :: HookStatus
        ,hValidity     :: Maybe HookValidity
        ,hEventType    :: EventType
        }
        deriving (Show,Eq,Ord,Typeable)

-- | to json as per MangoPay format
instance ToJSON Hook where
        toJSON h=objectSN ["Tag"  .= hTag h,"EventType" .= hEventType h,"Url" .= hUrl h,"Status" .= hStatus h]

-- | from json as per MangoPay format
instance FromJSON Hook where
        parseJSON (Object v) =Hook <$>
                         v .: "Id" <*>
                         v .: "CreationDate" <*>
                         v .: "Tag" <*>
                         v .: "Url" <*>
                         v .: "Status" <*>
                         v .: "Validity" <*>
                         v .: "EventType"
        parseJSON _=fail "Hook"
