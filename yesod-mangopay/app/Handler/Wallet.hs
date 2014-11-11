{-# LANGUAGE ConstraintKinds #-}
module Handler.Wallet where

import Import
import Web.MangoPay
import Yesod.MangoPay
import Control.Monad (join)
import Control.Arrow ((&&&))
import Data.Text (pack)

-- | get wallet list
getWalletsR :: AnyUserId -> Handler Html
getWalletsR uid=do
  -- no paging, should be reasonable
  wallets<-runYesodMPTToken $ getAll $ listWallets uid (ByCreationDate ASC)
  defaultLayout $ do
        setTitleI MsgTitleWallets
        $(widgetFile "wallets")

-- | get wallet creation form
getWalletR :: AnyUserId -> Handler Html
getWalletR uid = readerWallet uid Nothing

-- | get wallet edition form
getWalletEditR :: AnyUserId -> WalletId -> Handler Html
getWalletEditR uid wid = do
  wallet <- runYesodMPTToken $ fetchWallet wid
  readerWallet uid $ Just wallet


-- | helper to generate the proper form given maybe an existing wallet
readerWallet :: AnyUserId -> Maybe Wallet -> Handler Html
readerWallet uid mwallet = do
  (widget, enctype) <- generateFormPost $ walletForm mwallet
  let mwid = join $ wId <$> mwallet
  defaultLayout $ do
        setTitleI MsgTitleWallet
        $(widgetFile "wallet")


-- | helper to create or modify a wallet
helperWallet :: (Wallet -> AccessToken -> MangoPayT Handler Wallet) ->
  Maybe Wallet -> AnyUserId -> Handler Html
helperWallet fn mw uid=do
  ((result, _), _) <- runFormPost $ walletForm mw
  mwallet<-case result of
    FormSuccess w->do
            -- set the owner to current user
            let wo= w{wOwners=[uid]}
            catchMP (do
              wallet<-runYesodMPTToken $ fn wo
              setMessageI MsgWalletDone
              return (Just wallet)
              )
              (\e->do
                $(logError) $ pack $ show e
                setMessage $ toHtml $ show e
                return (Just wo)
              )
    _ -> do
            setMessageI MsgErrorData
            return Nothing
  readerWallet uid mwallet


postWalletR :: AnyUserId -> Handler Html
postWalletR = helperWallet createWallet Nothing


putWalletEditR :: AnyUserId -> WalletId -> Handler Html
putWalletEditR uid wid = do
  wallet <- runYesodMPTToken $ fetchWallet wid
  helperWallet modifyWallet (Just wallet) uid


-- | form for wallet
walletForm ::  HtmlForm Wallet
walletForm mwallet= renderDivs $ Wallet
    <$> aopt hiddenField "" (wId <$> mwallet)
    <*> pure (join $ wCreationDate <$> mwallet)
    <*> aopt textField (localizedFS MsgWalletCustomData) (wTag <$> mwallet)
    <*> pure []
    <*> areq textField (localizedFS MsgWalletDescription) (wDescription <$> mwallet)
    <*> areq (selectFieldList (map (id &&& id) $ maybe supportedCurrencies (\mw -> [wCurrency mw]) mwallet))
        (localizedFS MsgWalletCurrency) (wCurrency <$> mwallet)
    -- we can't edit the amount anyway, so we show it as disabled and return a const 0 value
    <*> (fmap (const $ Amount "EUR" 0) <$> aopt intField (disabled $ localizedFS MsgWalletBalance) (fmap aAmount <$> wBalance <$> mwallet))

