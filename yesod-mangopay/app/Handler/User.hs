{-# LANGUAGE ScopedTypeVariables #-}
-- | user handling
module Handler.User where

import Import
import Yesod.MangoPay
import Yesod.MangoPay.Util
import Web.MangoPay
import           Yesod.Form.Jquery
import Control.Monad (join, liftM)

getUserR :: AnyUserID -> Handler Html
getUserR uid=do
  eu <- runYesodMPTToken $ getUser uid
  case eu of
    (Left nu)->do
      let muser=Just nu
      (widget, enctype) <- generateFormPost $ naturalUserForm muser
      defaultLayout $ do
            aDomId <- newIdent
            setTitle "Manage a user"
            $(widgetFile "nuser")      
    (Right lu)->do
      let muser=Just lu
      (widget, enctype) <- generateFormPost $ legalUserForm muser
      defaultLayout $ do
            aDomId <- newIdent
            setTitle "Manage a user"
            $(widgetFile "luser")  

-- | get a natural user form
getNUserR :: Handler Html
getNUserR =do
  (muser,widget,enctype)<- userGet naturalUserForm fetchNaturalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "nuser")

-- | post a natural user form
postNUserR :: Handler Html
postNUserR = do
  (muser,widget,enctype)<- userPost naturalUserForm storeNaturalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "nuser")

-- | get a legal user form
getLUserR :: Handler Html
getLUserR = do
  (muser,widget,enctype)<- userGet legalUserForm fetchLegalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "luser")

-- | post a legal user form
postLUserR :: Handler Html
postLUserR = do
  (muser,widget,enctype)<- userPost legalUserForm storeLegalUser
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "luser")
 
userGet :: HtmlForm a
                 -> (Text -> AccessToken -> MangoPayT Handler a)
                 -> Handler (Maybe a, Widget, Enctype)
userGet form fetch =do
    muid<-lookupGetParam "id"
    muser<-case muid of
      Just uid->liftM Just $ runYesodMPTToken $ fetch uid
      _->return Nothing
    (widget, enctype) <- generateFormPost $ form muser
    return (muser,widget,enctype)

userPost :: HtmlForm a
                 -> (a -> AccessToken -> MangoPayT Handler a)
                 -> Handler (Maybe a, Widget, Enctype)
userPost form store = do
    ((result, _), _) <- runFormPost $ form Nothing
    (muser)<-case result of
              FormSuccess u -> do
                user<-runYesodMPTToken $ store u
                setMessage "User change done"
                return (Just user)
              _ -> do
                setMessage "Invalid data"
                return Nothing
    (widget, enctype) <- generateFormPost $ form muser
    return (muser,widget,enctype)


-- | form for natural user        
naturalUserForm ::  HtmlForm NaturalUser
naturalUserForm muser= renderDivs $ NaturalUser 
    <$> aopt hiddenField "" (uId <$> muser)
    <*> pure (join $ uCreationDate <$> muser)
    <*> areq textField (fs MsgUserEmail) (uEmail <$> muser)
    <*> areq textField (fs MsgUserFirst) (uFirstName <$> muser) 
    <*> areq textField (fs MsgUserLast) (uLastName <$> muser)
    <*> aopt textField (fs MsgUserAddress) (uAddress <$> muser) 
    <*> (day2Posix <$> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) (fs MsgUserBirthday) (posix2Day <$> uBirthday <$> muser))
    <*> areq textField (fs MsgUserNationality)  (uNationality <$> muser)
    <*> areq textField (fs MsgUserCountry) (uCountryOfResidence <$> muser)
    <*> aopt textField (fs MsgUserOccupation) (uOccupation <$> muser)
    <*> aopt (selectFieldList ranges) (fs MsgUserIncome) (uIncomeRange <$> muser)
    <*> aopt textField (fs MsgUserCustomData) (uTag <$> muser)
    <*> aopt textField (disabled $ fs MsgUserProofIdentity) (uProofOfIdentity <$> muser) -- value comes from Documents uploaded
    <*> aopt textField (disabled $ fs MsgUserProofAddress) (uProofOfAddress <$> muser) -- value comes from Documents uploaded
  where 

-- | form for legal user
legalUserForm :: HtmlForm LegalUser
legalUserForm muser= renderDivs $ LegalUser 
    <$> aopt hiddenField "" (lId <$> muser)
    <*> pure (join $ lCreationDate <$> muser)
    <*> areq textField (fs MsgUserEmail) (lEmail <$> muser)
    <*> areq textField (fs MsgUserName) (lName <$> muser) 
    <*> areq (selectFieldList ranges) (fs MsgUserPersonType) (lLegalPersonType <$> muser)
    <*> aopt textField (fs MsgUserHQAddress) (lHeadquartersAddress <$> muser)
    <*> areq textField (fs MsgUserRepFirst) (lLegalRepresentativeFirstName <$> muser)
    <*> areq textField (fs MsgUserRepLast) (lLegalRepresentativeLastName <$> muser)
    <*> aopt textField (fs MsgUserRepAddress) (lLegalRepresentativeAddress <$> muser)     
    <*> aopt textField (fs MsgUserRepEmail) (lLegalRepresentativeEmail <$> muser)  
    <*> (day2Posix <$> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) (fs MsgUserRepBirthday) (posix2Day <$> lLegalRepresentativeBirthday <$> muser))   
    <*> areq textField (fs MsgUserRepNationality) (lLegalRepresentativeNationality <$> muser)  
    <*> areq textField (fs MsgUserRepCountry) (lLegalRepresentativeCountryOfResidence <$> muser)
    <*> pure Nothing  -- value comes from Documents uploaded (I think)
    <*> aopt textField (fs MsgUserCustomData) (lTag <$> muser)  
    <*> pure Nothing  -- value comes from Documents uploaded
    <*> pure Nothing -- value comes from Documents uploaded

        
