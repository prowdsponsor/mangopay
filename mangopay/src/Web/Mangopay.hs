-- | API entry point
module Web.Mangopay (
        -- generic functions
        MangopayT
        ,runMangopayT
        ,runResourceInMp
        ,MpException
        
        -- useful types
        ,Credentials(..)
        ,AccessPoint(..)
        ,AccessToken(..)
        ,Pagination(..)
        
        -- access
        ,getPassphrase
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
        
        -- Events
        ,Event(..)
        ,EventType(..)
        ,EventSearchParams(..)
        ,searchEvents
)
where

import Web.Mangopay.Access
import Web.Mangopay.Events
import Web.Mangopay.Monad
import Web.Mangopay.Users
import Web.Mangopay.Types
import Web.Mangopay.Wallets