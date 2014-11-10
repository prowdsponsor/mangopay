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
        ,NaturalUserId
        ,LegalUser(..)
        ,LegalUserType(..)
        ,LegalUserId
        ,UserRef(..)
        ,PersonType(..)
        ,AnyUserId
        ,createNaturalUser
        ,modifyNaturalUser
        ,fetchNaturalUser
        ,createLegalUser
        ,modifyLegalUser
        ,fetchLegalUser
        ,getUser
        ,listUsers
        ,getExistingUserId

        -- Wallets
        ,Wallet(..)
        ,Amount(..)
        ,WalletId
        ,Currency
        ,createWallet
        ,modifyWallet
        ,fetchWallet
        ,listWallets
        ,Transfer(..)
        ,TransferId
        ,TransferStatus(..)
        ,Transaction(..)
        ,TransactionId
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
        ,HookId
        ,Hook(..)
        ,createHook
        ,modifyHook
        ,fetchHook
        ,listHooks
        ,eventFromQueryString
        ,eventFromQueryStringT

        -- Documents and pages
        ,Document(..)
        ,DocumentId
        ,DocumentType(..)
        ,DocumentStatus(..)
        ,createDocument
        ,modifyDocument
        ,fetchDocument
        ,createPage
        ,getKindOfAuthentication
        ,getRequiredDocumentTypes
        ,listDocuments
        ,listAllDocuments

        -- Accounts
        ,BankAccount(..)
        ,BankAccountId
        ,BankAccountDetails(..)
        ,PaymentType(..)
        ,createAccount
        ,fetchAccount
        ,listAccounts

        -- Payins
        ,PaymentExecution(..)
        ,BankWireId
        ,BankWire(..)
        ,createBankWirePayIn
        ,fetchBankWirePayIn
        ,mkBankWire
        ,CardPayinId
        ,CardPayin(..)
        ,createCardPayin
        ,fetchCardPayin
        ,mkCardPayin

        -- Payouts
        ,PayoutId
        ,Payout(..)
        ,mkPayout
        ,createPayout
        ,fetchPayout

        -- Cards
        ,CardRegistration(..)
        ,CardRegistrationId
        ,CardId
        ,CardInfo(..)
        ,Card(..)
        ,CardValidity(..)
        ,mkCardRegistration
        ,createCardRegistration
        ,modifyCardRegistration
        ,fetchCard
        ,listCards

        -- Refunds
        ,RefundId
        ,Refund(..)
        ,RefundRequest(..)
        ,RefundReason(..)
        ,RefundReasonType(..)
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
