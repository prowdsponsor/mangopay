-- | transactions
module Handler.Transaction where

import Import

import Yesod.MangoPay
import Web.MangoPay

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