-- | transactions: pay-in, transfer, pay-out
module Handler.Transaction where

import Import

import Yesod.MangoPay
import Web.MangoPay
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Data.Text (pack)

-- | transaction list
getTransactionsR :: AnyUserID -> Handler Html
getTransactionsR uid= do
    pg<-getPagination
    txsL<-runYesodMPTToken $ listTransactionsForUser uid pg
    -- pagination links
    let (previous,next)=getPaginationNav pg txsL
    let txs=plData txsL
    defaultLayout $ do
        setTitleI MsgTitleTransactions
        $(widgetFile "transactions")


-- | get payin form
getPayinR :: AnyUserID -> Handler Html
getPayinR uid=do
    cards<-runYesodMPTToken $ getAll $ listCards uid
    wallets<-runYesodMPTToken $ getAll $ listWallets uid
    (widget, enctype) <- generateFormPost $ payinInForm cards wallets
    defaultLayout $ do
        setTitleI MsgTitlePayIn
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
              _<-runYesodMPTToken $ createCardPayin cpi
              setMessageI MsgPayInDone
              redirect $ TransactionsR uid
              )
              (\e->do
                setMessage $ toHtml $ show e
                defaultLayout $ do
                  setTitleI MsgTitlePayIn
                  $(widgetFile "payin")
              )
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  setTitleI MsgTitlePayIn
                  $(widgetFile "payin")

-- | get first transfer page: choose the target user
getTransfer1R :: AnyUserID -> Handler Html
getTransfer1R uid=do
  users<-runYesodMPTToken $ getAll listUsers
  defaultLayout $ do
        setTitleI MsgTitleTransfer
        $(widgetFile "transfer1")

-- | get second transfer page: choose between wallets
getTransfer2R :: AnyUserID -> AnyUserID -> Handler Html
getTransfer2R uid touid=do
    fromWallets<-runYesodMPTToken $ getAll $ listWallets uid
    toWallets<-runYesodMPTToken $ getAll $ listWallets touid
    (widget, enctype) <- generateFormPost $ transferForm fromWallets toWallets
    defaultLayout $ do
        setTitleI MsgTitleTransfer
        $(widgetFile "transfer2")

-- | perfrm transfer
postTransfer2R :: AnyUserID -> AnyUserID -> Handler Html
postTransfer2R uid touid=do
  fromWallets<-runYesodMPTToken $ getAll $ listWallets uid
  toWallets<-runYesodMPTToken $ getAll $ listWallets touid

  ((result, widget), enctype) <- runFormPost $ transferForm fromWallets toWallets
  case result of
    FormSuccess (MPTransfer from to am cur)->do
            let t1=Web.MangoPay.Transfer Nothing Nothing Nothing uid (Just touid) (Amount cur am) (Amount cur 0)
                        from to Nothing Nothing Nothing Nothing Nothing
            catchMP (do
              _<-runYesodMPTToken $ createTransfer t1
              setMessageI MsgTransferDone
              redirect $ TransactionsR uid
              )
              (\e->do
                setMessage $ toHtml $ show e
                defaultLayout $ do
                  setTitleI MsgTitleTransfer
                  $(widgetFile "transfer2")
              )
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  setTitleI MsgTitleTransfer
                  $(widgetFile "transfer2")

-- | get payout form
getPayoutR :: AnyUserID -> Handler Html
getPayoutR uid=do
    wallets<-runYesodMPTToken $ getAll $ listWallets uid
    accounts<-runYesodMPTToken $ getAll $ listAccounts uid

    (widget, enctype) <- generateFormPost $ payoutForm wallets accounts
    defaultLayout $ do
        setTitleI MsgTitlePayOut
        $(widgetFile "payout")

-- | payout
postPayoutR :: AnyUserID -> Handler Html
postPayoutR uid=do
  wallets<-runYesodMPTToken $ getAll $ listWallets uid
  accounts<-runYesodMPTToken $ getAll $ listAccounts uid

  ((result, widget), enctype) <- runFormPost $ payoutForm wallets accounts
  case result of
    FormSuccess (PayOut wid aid am cur)->do
            let po=mkPayout uid wid (Amount cur am) (Amount cur 0) aid
            catchMP (do
              _<-runYesodMPTToken $ createPayout po
              setMessageI MsgPayOutDone
              redirect $ TransactionsR uid
              )
              (\e->do
                setMessage $ toHtml $ show e
                defaultLayout $ do
                  setTitleI MsgTitlePayOut
                  $(widgetFile "payout")
              )
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  setTitleI MsgTitlePayOut
                  $(widgetFile "payout")



-- | data necessary for payin
data PayIn = PayIn CardID WalletID Integer Currency

-- | payin form
payinInForm :: [Card] -> [Wallet] -> Html -> MForm Handler (FormResult PayIn, Widget)
payinInForm cards wallets= renderDivs $ PayIn
  <$> areq (selectFieldList (map (cAlias &&& cId) cards)) (localizedFS MsgPayInCard) Nothing
  <*> areq (selectFieldList (map (wDescription &&& (fromJust . wId)) wallets)) (localizedFS MsgPayInWallet) Nothing
  <*> areq intField (localizedFS MsgPayInAmount) Nothing
  <*> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgPayInCurrency) Nothing

-- | data necessary for transfer
data MPTransfer= MPTransfer WalletID WalletID Integer Currency

-- | transfer form
transferForm :: [Wallet] -> [Wallet] -> Html -> MForm Handler (FormResult MPTransfer, Widget)
transferForm fromWallets toWallets=renderDivs $ MPTransfer
  <$> areq (selectFieldList (map (wDescription &&& (fromJust . wId)) fromWallets)) (localizedFS MsgTransferFromWallet) Nothing
  <*> areq (selectFieldList (map (wDescription &&& (fromJust . wId)) toWallets)) (localizedFS MsgTransferToWallet) Nothing
  <*> areq intField (localizedFS MsgTransferAmount) Nothing
  <*> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgTransferCurrency) Nothing

-- | data necessary for payout
data PayOut = PayOut WalletID BankAccountID Integer Currency

-- | payin form
payoutForm :: [Wallet] -> [BankAccount] -> Html -> MForm Handler (FormResult PayOut, Widget)
payoutForm wallets accounts= renderDivs $ PayOut
  <$> areq (selectFieldList (map (wDescription &&& (fromJust . wId)) wallets)) (localizedFS MsgPayOutWallet) Nothing
  <*> areq (selectFieldList (map ((pack . show . baDetails) &&& (fromJust . baId)) accounts)) (localizedFS MsgPayOutAccount) Nothing
  <*> areq intField (localizedFS MsgPayOutAmount) Nothing
  <*> areq (selectFieldList (map (id &&& id) supportedCurrencies)) (localizedFS MsgPayOutCurrency) Nothing

