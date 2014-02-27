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
        ,storeNaturalUser
        ,fetchNaturalUser
        ,storeLegalUser
        ,fetchLegalUser
        ,getUser
        ,listUsers
)
where

import Web.Mangopay.Access
import Web.Mangopay.Monad
import Web.Mangopay.Users
import Web.Mangopay.Types