{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
-- | handle users
module Web.Mangopay.Users where

import Web.Mangopay.Monad
import Web.Mangopay.Types

import Data.Conduit
import Data.Text
import Data.Typeable (Typeable)
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Applicative
import qualified Network.HTTP.Types as HT

-- | create or edit a natural user
storeNaturalUser ::  (MonadBaseControl IO m, MonadResource m) => NaturalUser -> AccessToken -> MangopayT m NaturalUser
storeNaturalUser u at= 
        case uId u of
                Nothing-> do
                        url<-getClientURL "/users/natural"
                        postExchange url (Just at) u 
                Just i-> do
                        url<-getClientURLMultiple ["/users/natural/",i]
                        putExchange url (Just at) u{uProofOfIdentity=Nothing,uProofOfAddress=Nothing} 
                

-- | fetch a natural user from her ID
fetchNaturalUser :: (MonadBaseControl IO m, MonadResource m) => NaturalUserID -> AccessToken -> MangopayT m NaturalUser
fetchNaturalUser uid at=do
        url<-getClientURLMultiple ["/users/natural/",uid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req 

-- | create or edit a natural user
storeLegalUser ::  (MonadBaseControl IO m, MonadResource m) => LegalUser -> AccessToken -> MangopayT m LegalUser
storeLegalUser u at= 
        case lId u of
                Nothing-> do
                        url<-getClientURL "/users/legal"
                        postExchange url (Just at) u 
                Just i-> do
                        url<-getClientURLMultiple ["/users/legal/",i]
                        putExchange url (Just at) u
                

-- | fetch a natural user from her ID
fetchLegalUser :: (MonadBaseControl IO m, MonadResource m) => LegalUserID -> AccessToken -> MangopayT m LegalUser
fetchLegalUser uid at=do
        url<-getClientURLMultiple ["/users/legal/",uid]
        req<-getGetRequest url (Just at) ([]::HT.Query)
        getJSONResponse req 

-- not supported
--deleteNaturalUser :: (MonadBaseControl IO m, MonadResource m) => UserID -> AccessToken -> MangopayT m ()
--deleteNaturalUser uid at=do
--        url<-getClientURL (TE.encodeUtf8 $ Data.Text.concat ["/users/natural/",uid])
--        req<-getDeleteRequest url (Just at) ([]::HT.Query)
--        _::Object<- getJSONResponse req 
--        return ()

-- | User ID
type NaturalUserID = Text

-- | supported income ranges
data IncomeRange=IncomeRange1 | IncomeRange2 | IncomeRange3 | IncomeRange4 | IncomeRange5 | IncomeRange6
      deriving (Show,Read,Eq,Ord,Typeable)   

-- | to json as per Mangopay format
instance ToJSON IncomeRange  where
    toJSON IncomeRange1="1"
    toJSON IncomeRange2="2"
    toJSON IncomeRange3="3"
    toJSON IncomeRange4="4"
    toJSON IncomeRange5="5"
    toJSON IncomeRange6="6"
 
-- | from json as per Mangopay format
instance FromJSON IncomeRange where
    parseJSON (String "1") =pure IncomeRange1                  
    parseJSON (String "2") =pure IncomeRange2 
    parseJSON (String "3") =pure IncomeRange3  
    parseJSON (String "4") =pure IncomeRange4  
    parseJSON (String "5") =pure IncomeRange5  
    parseJSON (String "6") =pure IncomeRange6                 
    parseJSON _= fail "IncomeRange"    

-- | a natural user
-- <http://docs.mangopay.com/api-references/users/natural-users/>
data NaturalUser=NaturalUser {
        uEmail :: Text -- ^ User’s e-mail
        ,uFirstName :: Text -- ^ User’s firstname
        ,uLastName :: Text -- ^  User’s lastname
        ,uAddress :: Maybe Text -- ^  User’s address
        ,uBirthday :: POSIXTime -- ^   User’s birthdate
        ,uNationality :: Text -- ^ User’s Nationality
        ,uCountryOfResidence:: Text -- ^User’s country of residence
        ,uOccupation :: Maybe Text -- ^User’s occupation (ie. Work)
        ,uIncomeRange :: Maybe IncomeRange -- ^ User’s income range
        ,uTag :: Maybe Text -- ^  Custom data
        ,uProofOfIdentity :: Maybe Text -- ^  
        ,uProofOfAddress  :: Maybe Text -- ^  
        ,uCreationDate   :: Maybe POSIXTime -- ^  The creation date of the user object
        ,uId      :: Maybe NaturalUserID -- ^  The Id of the object
        }
     deriving (Show,Eq,Ord,Typeable)
     
-- | to json as per Mangopay format    
instance ToJSON NaturalUser  where
    toJSON u=object ["Tag" .= uTag u,"Email" .= uEmail u,"FirstName".= uFirstName u,"LastName" .= uLastName u,"Address" .= uAddress u, "Birthday" .=  uBirthday u
      ,"Nationality" .= uNationality u,"CountryOfResidence" .= uCountryOfResidence u,"Occupation" .= uOccupation u, "IncomeRange" .= uIncomeRange u,"ProofOfIdentity" .= uProofOfIdentity u
      ,"ProofOfAddress" .= uProofOfAddress u,"PersonType" .= String "NATURAL"] 

-- | from json as per Mangopay format
instance FromJSON NaturalUser where
    parseJSON (Object v) =NaturalUser <$>
                         v .: "Email" <*>
                         v .: "FirstName" <*>
                         v .: "LastName" <*>
                         v .:? "Address" <*>
                         v .: "Birthday" <*>
                         v .: "Nationality" <*>
                         v .: "CountryOfResidence" <*>
                         v .:? "Occupation" <*>
                         v .:? "IncomeRange" <*>
                         v .:? "Tag" <*>
                         v .:? "ProofOfIdentity" <*>
                         v .:? "ProofOfAddress" <*>
                         v .: "CreationDate" <*>
                         v .: "Id" 
    parseJSON _= fail "NaturalUser"  

-- | User ID
type LegalUserID = Text
 
-- | the type of legal user    
data LegalUserType = Business | Organization
      deriving (Show,Read,Eq,Ord,Typeable)   
    
-- | to json as per Mangopay format
instance ToJSON LegalUserType  where
    toJSON Business="BUSINESS"
    toJSON Organization="ORGANIZATION"
 
-- | from json as per Mangopay format
instance FromJSON LegalUserType where
    parseJSON (String "BUSINESS") =pure Business                  
    parseJSON (String "ORGANIZATION") =pure Organization                
    parseJSON _= fail "LegalUserType"       
 
-- | a legal user
-- <http://docs.mangopay.com/api-references/users/legal-users/>    
data LegalUser=LegalUser {
        lEmail :: Text -- ^ The email of the company or the organization
        ,lName :: Text -- ^ The name of the company or the organization
        ,lLegalPersonType :: LegalUserType -- ^ The type of the legal user (‘BUSINESS’ or ’ORGANIZATION’)
        ,lHeadquartersAddress  :: Maybe Text -- ^ The address of the company’s headquarters
        ,lLegalRepresentativeFirstName :: Text -- ^ The firstname of the company’s Legal representative person
        ,lLegalRepresentativeLastName :: Text -- ^ The lastname of the company’s Legal representative person
        ,lLegalRepresentativeAdress :: Maybe Text -- ^ The address of the company’s Legal representative person
        ,lLegalRepresentativeEmail :: Maybe Text -- ^  The email of the company’s Legal representative person
        ,lLegalRepresentativeBirthday :: POSIXTime -- ^ The birthdate of the company’s Legal representative person
        ,lLegalRepresentativeNationality :: Text -- ^ the nationality of the company’s Legal representative person
        ,lLegalRepresentativeCountryOfResidence :: Text -- ^  The country of residence of the company’s Legal representative person
        ,lStatute  :: Maybe Text -- ^  The business statute of the company
        ,lTag   :: Maybe Text -- ^  Custom data
        ,lProofOfRegistration :: Maybe Text -- ^   The proof of registration of the company
        ,lShareholderDeclaration   :: Maybe Text -- ^   The shareholder declaration of the company
        ,lCreationDate    :: Maybe POSIXTime -- ^ The creation date of the user object
        ,lId       :: Maybe Text -- ^   The Id of the object
        }
        deriving (Show,Eq,Ord,Typeable)
        
-- | to json as per Mangopay format    
instance ToJSON LegalUser  where
    toJSON u=object ["Tag" .= lTag u,"Email" .= lEmail u,"Name".= lName u,"LegalPersonType" .= lLegalPersonType u,"HeadquartersAddress" .= lHeadquartersAddress u, "LegalRepresentativeFirstName" .=  lLegalRepresentativeFirstName u
      ,"LegalRepresentativeLastName" .= lLegalRepresentativeLastName u,"LegalRepresentativeAdress" .= lLegalRepresentativeAdress u,"LegalRepresentativeEmail" .= lLegalRepresentativeEmail u, "LegalRepresentativeBirthday" .= lLegalRepresentativeBirthday u,"LegalRepresentativeNationality" .= lLegalRepresentativeNationality u
      ,"LegalRepresentativeCountryOfResidence" .= lLegalRepresentativeCountryOfResidence u,"Statute" .= lStatute u,"ProofOfRegistration" .=lProofOfRegistration u,"ShareholderDeclaration" .=lShareholderDeclaration u,"PersonType" .= String "LEGAL"]        
      
-- | from json as per Mangopay format
instance FromJSON LegalUser where
    parseJSON (Object v) =LegalUser <$>
                         v .: "Email" <*>
                         v .: "Name" <*>
                         v .: "LegalPersonType" <*>
                         v .:? "HeadquartersAddress" <*>
                         v .: "LegalRepresentativeFirstName" <*>
                         v .: "LegalRepresentativeLastName" <*>
                         v .:? "LegalRepresentativeAdress" <*>
                         v .:? "LegalRepresentativeEmail" <*>
                         v .: "LegalRepresentativeBirthday" <*>
                         v .: "LegalRepresentativeNationality" <*>
                         v .: "LegalRepresentativeCountryOfResidence" <*>
                         v .:? "Statute" <*>
                         v .:? "Tag" <*>
                         v .:? "ProofOfRegistration" <*>
                         v .:? "ShareholderDeclaration" <*>
                         v .: "CreationDate" <*>
                         v .: "Id" 
    parseJSON _= fail "NaturalUser" 
     