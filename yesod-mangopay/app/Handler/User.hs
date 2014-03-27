{-# LANGUAGE ScopedTypeVariables #-}
-- | user handling
module Handler.User where

import Import
import Yesod.MangoPay
import Yesod.MangoPay.Util
import Web.MangoPay
import           Yesod.Form.Jquery
import Control.Monad (join, liftM)

-- | get a natural user form
getNUserR :: Handler Html
getNUserR =do
  (muser,widget,enctype,msg)<- userGet naturalUserForm fetchNaturalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "nuser")

-- | post a natural user form
postNUserR :: Handler Html
postNUserR = do
  (muser,widget,enctype,msg)<- userPost naturalUserForm storeNaturalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "nuser")

-- | get a legal user form
getLUserR :: Handler Html
getLUserR = do
  (muser,widget,enctype,msg)<- userGet legalUserForm fetchLegalUser 
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "luser")

-- | post a legal user form
postLUserR :: Handler Html
postLUserR = do
  (muser,widget,enctype,msg)<- userPost legalUserForm storeLegalUser
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Manage a user"
        $(widgetFile "luser")
 
userGet :: HtmlForm a
                 -> (Text -> AccessToken -> MangoPayT Handler a)
                 -> Handler (Maybe a, Widget, Enctype, Text)
userGet form fetch =do
    muid<-lookupGetParam "id"
    muser<-case muid of
      Just uid->liftM Just $ runYesodMPTToken $ fetch uid
      _->return Nothing
    (widget, enctype) <- generateFormPost $ form muser
    let msg=""::Text
    return (muser,widget,enctype,msg)

userPost :: HtmlForm a
                 -> (a -> AccessToken -> MangoPayT Handler a)
                 -> Handler (Maybe a, Widget, Enctype, Text)
userPost form store = do
    ((result, _), _) <- runFormPost $ form Nothing
    (muser,msg::Text)<-case result of
              FormSuccess u -> do
                user<-runYesodMPTToken $ store u
                return (Just user,"User change done")
              _ -> return (Nothing,"Invalid data")
    (widget, enctype) <- generateFormPost $ form muser
    return (muser,widget,enctype,msg)


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
    <*> pure Nothing -- value comes from Documents uploaded
    <*> pure Nothing -- value comes from Documents uploaded
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

        
