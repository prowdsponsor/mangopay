-- | account handling
module Handler.Account where

import Import
import Web.MangoPay
import Yesod.MangoPay


-- | get account list
getAccountsR :: AnyUserID -> Handler Html
getAccountsR uid=do
  -- no paging, should be reasonable
  accounts<-runYesodMPTToken $ getAll $ listAccounts uid
  defaultLayout $ do
        setTitleI MsgTitleAccounts
        $(widgetFile "accounts")

-- | get account registration form
getAccountR :: AnyUserID -> Handler Html
getAccountR uid=do
    (widget, enctype) <- generateFormPost accountForm
    defaultLayout $ do
        setTitleI MsgTitleAccount
        $(widgetFile "account")

-- | register account
postAccountR :: AnyUserID -> Handler Html
postAccountR uid=do
  ((result, widget), enctype) <- runFormPost accountForm
  case result of
    FormSuccess bap->
            catchMP (do
              _<-runYesodMPTToken $ createAccount (toBankAccount uid bap)
              setMessageI MsgAccountDone
              redirect $ AccountsR uid
              )
               (\e->do
                setMessage $ toHtml $ show e
                defaultLayout $ do
                  setTitleI MsgTitleAccount
                  $(widgetFile "account")
                  )
    _ -> do
            setMessageI MsgErrorData
            defaultLayout $ do
                  setTitleI MsgTitleAccount
                  $(widgetFile "account")

-- | partial data for account
data BankAccountPartial=BankAccountPartial {
   bapTag :: Maybe Text
  ,bapIBAN :: Text
  ,bapBIC :: Text
  ,bapOwnerName :: Text
  ,bapOwnerAddress :: Maybe Text
  }

-- | get the proper BankAccount structure
toBankAccount :: AnyUserID -> BankAccountPartial -> BankAccount
toBankAccount uid bap=BankAccount Nothing Nothing (Just uid) (bapTag bap) (IBAN (bapIBAN bap) (bapBIC bap))
  (bapOwnerName bap) (bapOwnerAddress bap)

-- | form for bank account
accountForm :: Html -> MForm Handler (FormResult BankAccountPartial, Widget)
accountForm = renderDivs $ BankAccountPartial
  <$> aopt textField (localizedFS MsgAccountCustomData) Nothing
  <*> areq textField (localizedFS MsgAccountIBAN) Nothing
  <*> areq textField (localizedFS MsgAccountBIC) Nothing
  <*> areq textField (localizedFS MsgAccountOwnerName) Nothing
  <*> aopt textField (localizedFS MsgAccountOwnerAddress) Nothing
