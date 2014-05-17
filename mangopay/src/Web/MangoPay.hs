-- | API entry point
module Web.MangoPay (
        -- generic functions
        MangoPayT
        ,runMangoPayT
        ,runResourceInMp
        ,MpException
        ,getAll
        
        -- useful types
        ,Credentials(..)
        ,AccessPoint(..)
        ,AccessToken(..)
        ,OAuthToken(..)
        ,Pagination(..)
        ,PagedList(..)
        ,MPUsableMonad
        ,ToHtQuery(..)
        ,CardExpiration(..)
        ,readCardExpiration
        ,writeCardExpiration
        ,KindOfAuthentication(..)
        
        -- access
        ,createCredentialsSecret
        ,oauthLogin
        ,toAccessToken
        
        -- Users
        ,NaturalUser(..)
        ,IncomeRange(..)
        ,incomeBounds
        ,incomeRange
        ,NaturalUserID
        ,LegalUser(..)
        ,LegalUserType(..)
        ,LegalUserID
        ,UserRef(..)
        ,PersonType(..)
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
        ,Transaction(..)
        ,TransactionID
        ,TransactionType(..)
        ,TransactionNature(..)
        ,createTransfer
        ,fetchTransfer
        ,listTransactions
        ,listTransactionsForUser
        
        -- Events and Hooks
        ,Event(..)
        ,EventType(..)
        ,EventSearchParams(..)
        ,searchEvents
        ,searchAllEvents
        ,checkEvent
        ,HookStatus(..)
        ,HookValidity(..)
        ,HookID
        ,Hook(..)
        ,storeHook
        ,fetchHook
        ,listHooks
        ,eventFromQueryString
        ,eventFromQueryStringT
        
        -- Documents and pages
        ,Document(..)
        ,DocumentID
        ,DocumentType(..)
        ,DocumentStatus(..)
        ,storeDocument
        ,fetchDocument
        ,storePage
        ,getKindOfAuthentication
        
        -- Accounts
        ,BankAccount(..)
        ,BankAccountID
        ,BankAccountDetails(..)
        ,PaymentType(..)
        ,storeAccount
        ,fetchAccount
        ,listAccounts
        
        -- Payins
        ,PaymentExecution(..)
        ,BankWireID
        ,BankWire(..)
        ,storeBankWire
        ,fetchBankWire
        ,mkBankWire
        ,CardPayinID
        ,CardPayin(..)
        ,storeCardPayin
        ,fetchCardPayin
        ,mkCardPayin
        
        -- Payouts
        ,PayoutID
        ,Payout(..)
        ,mkPayout
        ,storePayout
        ,fetchPayout
                
        -- Cards
        ,CardRegistration(..)
        ,CardRegistrationID
        ,CardID
        ,CardInfo(..)
        ,Card(..)
        ,CardValidity(..)
        ,mkCardRegistration
        ,storeCardRegistration
        ,fetchCard
        ,listCards
        
        -- Refunds
        ,RefundID
        ,Refund(..)
        ,RefundRequest(..)
        ,refundTransfer
        ,refundPayin
        ,fetchRefund
)
where

import Web.MangoPay.Access
import Web.MangoPay.Accounts
import Web.MangoPay.Cards
import Web.MangoPay.Documents
import Web.MangoPay.Events
import Web.MangoPay.Monad
import Web.MangoPay.Payins
import Web.MangoPay.Payouts
import Web.MangoPay.Refunds
import Web.MangoPay.Users
import Web.MangoPay.Types
import Web.MangoPay.Wallets