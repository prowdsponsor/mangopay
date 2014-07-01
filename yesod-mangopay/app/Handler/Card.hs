-- | cards handling
module Handler.Card where

import Import
import Web.MangoPay
import Yesod.MangoPay
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import Data.ByteString.Lazy (fromChunks)
import Control.Monad (liftM)
import Network.Wai (rawQueryString)
import Data.Text (pack)
import Control.Arrow ((&&&))

-- | get card list
getCardsR :: AnyUserID -> Handler Html
getCardsR uid=do
  -- no paging, should be reasonable
  cards<-runYesodMPTToken $ getAll $ listCards uid
  ((_,widget), enctype) <- generateFormGet currencyForm
  defaultLayout $ do
        setTitleI MsgTitleCards
        $(widgetFile "cards")

-- | get card registration form
-- this form will not be sent to this server, but to the validation server!
-- we have an iframe in that page
-- the process is
-- 1. create a pending registration in Mangopay
-- 2. generate a credit card detail form sending data to the validation server, and a return url pointing to Card2R
-- 3. get the result in Card2R, dump it into the iframe
-- 4. get the iframe data via Javascript and submit it to CardR
-- The use of the iframe means the user only sees our page, not the validation server's
-- The use of Card2R ensure the iframe is considered by the browser as being populated by this server
-- So we can pick its contents via Javascript without breaking cross-domain security
-- and submit it back to us using the same session!
-- if we didn't have a iframe and redirect the main page via the validation server, we lose the session, so we would need to encode a token in the url
-- we can also use the pure Ajax solution via the mangopay JS toolkit, but the iframe system should work in more browsers
getCardR :: AnyUserID -> Handler Html
getCardR uid=do
    ((result, _), _) <- runFormGet currencyForm
    case result of
      FormSuccess curr->catchW $ do
        let cr1=mkCardRegistration uid curr
        -- step 1: create pending registration
        cr2<-runYesodMPTToken $ createCardRegistration cr1
        let Just url = crCardRegistrationURL cr2 -- the url of the validation server
            Just pre = crPreregistrationData cr2
            Just ak = crAccessKey cr2
        -- we keep the registration info in the session
        setSession "cardReg" $ toStrict $ toLazyText $ encodeToTextBuilder $ toJSON cr2
        -- generate hidden form
        (widget, enctype) <- generateFormPost cardTokenForm

        defaultLayout $ do
            -- JQuery is useful!
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
            setTitleI MsgTitleCard
            $(widgetFile "card")
      f ->do
        $(logError) $ pack $ show f
        setMessageI MsgErrorData
        redirect $ CardsR uid

-- | this only dumps the query string
-- this is used as the returnURL for the validation server
-- so that the iframe will be populated by content coming from this server
-- which allows Javascript to use it
getCard2R :: Handler TypedContent
getCard2R =do
  qs<-liftM rawQueryString waiRequest
  respond typePlain qs

-- | this gets the token via JavaScript submission
postCardR :: AnyUserID -> Handler Html
postCardR uid=do
  ((result, _), _) <- runFormPost cardTokenForm
  mjcr<-lookupSession "cardReg"
  deleteSession "cardReg"
  case result of
    FormSuccess (Token dat)->
      case mjcr of
        Nothing->do
          $(logError) $ "lookupSession->Nothing"
          setMessageI MsgErrorData
          redirect $ CardR uid
        Just jcr->do
          let ecr=eitherDecode $ fromChunks [TE.encodeUtf8 jcr]
          case ecr of
            Right cr->do
              _<-runYesodMPTToken $ modifyCardRegistration cr{crRegistrationData=Just dat}
              setMessageI MsgCardDone
              redirect $ CardsR uid
            Left err->do
              $(logError) $ pack err
              setMessageI MsgErrorData
              redirect $ CardR uid
    f ->do
        $(logError) $ pack $ show f
        setMessageI MsgErrorData
        redirect $ CardR uid

-- | token for card registration
data Token=Token Text
    deriving Show

-- | simple form for card registration
-- the field is hidden and populated via JavaScript (card.julius)
cardTokenForm ::   Html -> MForm Handler (FormResult Token, Widget)
cardTokenForm = renderDivs $ Token
  <$> areq hiddenField (FieldSettings "" Nothing (Just "frmdat") (Just "frmdat") []) Nothing

-- | simple form for choosing the currency of the card registration
currencyForm :: Html -> MForm Handler (FormResult Currency, Widget)
currencyForm = renderDivs $ id
  <$> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgCardCurrency) (Just "EUR")

