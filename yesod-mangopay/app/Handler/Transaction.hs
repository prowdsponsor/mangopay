-- | transactions
module Handler.Transaction where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

-- | transaction list
getTransactionsR :: AnyUserID -> Handler Html
getTransactionsR uid= do
    pg<-getPagination
    txsL<-runYesodMPTToken $ listTransactionsForUser uid pg
    -- pagination links
    let (previous,next)=getPaginationNav pg txsL
    let txs=plData txsL
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitleTransactions
        $(widgetFile "transactions")
        

-- | get payin form
getPayinR :: AnyUserID -> Handler Html
getPayinR uid=do
    cards<-runYesodMPTToken $ getAll $ listCards uid
    wallets<-runYesodMPTToken $ getAll $ listWallets uid
    (widget, enctype) <- generateFormPost $ payinInForm cards wallets
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitleCard
        $(widgetFile "payin")

-- | payin
postPayinR :: AnyUserID -> Handler Html
postPayinR uid=do
  cards<-runYesodMPTToken $ getAll $ listCards uid
  wallets<-runYesodMPTToken $ getAll $ listWallets uid
    
  ((result, widget), enctype) <- runFormPost $ payinInForm cards wallets
  case result of
    FormSuccess (PayIn cid wid am cur)->do
            let cpi=mkCardPayin uid uid wid (Amount cur am) (Amount cur 0) "http://dummy" cid
            catchMP (do
              _<-runYesodMPTToken $ storeCardPayin cpi
              setMessageI MsgPayInDone
              redirect $ TransactionsR uid
              )
              (\e->do
                setMessage $ toHtml $ show e
                defaultLayout $ do
                  aDomId <- newIdent
                  setTitleI MsgTitleWallet
                  $(widgetFile "payin")
              )    
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  aDomId <- newIdent
                  setTitleI MsgTitleWallet
                  $(widgetFile "payin")

-- | data necessary for payin
data PayIn = PayIn CardID WalletID Integer Currency

-- | payin form
payinInForm :: [Card] -> [Wallet] -> Html -> MForm Handler (FormResult PayIn, Widget)
payinInForm cards wallets= renderDivs $ PayIn
  <$> areq (selectFieldList (map (cAlias &&& cId) cards)) (localizedFS MsgPayInCard) Nothing
  <*> areq (selectFieldList (map (wDescription &&& (fromJust . wId)) wallets)) (localizedFS MsgPayInWallet) Nothing
  <*> areq intField (localizedFS MsgPayInAmount) Nothing
  <*> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgPayInCurrency) Nothing