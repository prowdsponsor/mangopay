-- | cards handling
module Handler.Card where

import Import
import Web.MangoPay
import Yesod.MangoPay
import Control.Arrow ((&&&))

-- | get card list
getCardsR :: AnyUserID -> Handler Html
getCardsR uid=do
  -- no paging, should be reasonable
  cards<-runYesodMPTToken $ getAll $ listCards uid
  defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitleCards
        $(widgetFile "cards")
        
-- | get card registration form
getCardR :: AnyUserID -> Handler Html
getCardR uid=do
    (widget, enctype) <- generateFormPost cardInfoForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitleCard
        $(widgetFile "card")

-- | register card
postCardR :: AnyUserID -> Handler Html
postCardR uid=do
  ((result, widget), enctype) <- runFormPost cardInfoForm
  case result of
    FormSuccess (c,ci)->do
            _<-runYesodMPTToken $ fullRegistration uid c ci
            setMessageI MsgCardDone
            redirect $ CardsR uid
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  aDomId <- newIdent
                  setTitleI MsgTitleCard
                  $(widgetFile "card")
        
cardInfoForm :: Html -> MForm Handler (FormResult (Currency,CardInfo), Widget)
cardInfoForm = renderDivs $ (\a b c d-> (a,CardInfo b c d))
  <$> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgCardCurrency) Nothing
  <*> areq textField (localizedFS MsgCardNumber) Nothing
  <*> areq textField (localizedFS MsgCardExpire) Nothing
  <*> areq textField (localizedFS MsgCardCSC) Nothing  