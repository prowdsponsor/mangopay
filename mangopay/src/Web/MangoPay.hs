-- | API entry point
module Web.MangoPay (
        -- generic functions
        MangoPayT
        ,runMangoPayT
        ,runResourceInMp
        ,MpException
        
        -- useful types
        ,Credentials(..)
        ,AccessPoint(..)
        ,AccessToken(..)
        ,Pagination(..)
        
        -- access
        ,createCredentialsSecret
        ,oauthLogin
        
        -- Users
        ,NaturalUser(..)
        ,IncomeRange(..)
        ,NaturalUserID
        ,LegalUser(..)
        ,LegalUserType(..)
        ,LegalUserID
        ,UserRef(..)
        ,AnyUserID
        ,storeNaturalUser
        ,fetchNaturalUser
        ,storeLegalUser
        ,fetchLegalUser
        ,getUser
        ,listUsers
        
        -- Wallets
        ,Wallet(..)
        ,Amount(..)
        ,WalletID
        ,Currency
        ,storeWallet
        ,fetchWallet
        ,listWallets
        ,Transfer(..)
        ,TransferID
        ,TransferStatus(..)
        ,createTransfer
        ,fetchTransfer
        ,listTransfers
        ,listTransfersForUser
        
        -- Events and Hooks
        ,Event(..)
        ,EventType(..)
        ,EventSearchParams(..)
        ,searchEvents
        ,HookStatus(..)
        ,HookValidity(..)
        ,HookID
        ,Hook(..)
        ,storeHook
        ,fetchHook
        ,listHooks
        ,eventFromQueryString
        
        -- Documents and pages
        ,Document(..)
        ,DocumentID
        ,DocumentType(..)
        ,DocumentStatus(..)
        ,storeDocument
        ,fetchDocument
        ,storePage
)
where

import Web.MangoPay.Access
import Web.MangoPay.Documents
import Web.MangoPay.Events
import Web.MangoPay.Monad
import Web.MangoPay.Users
import Web.MangoPay.Types
import Web.MangoPay.Wallets