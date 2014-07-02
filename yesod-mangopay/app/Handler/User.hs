{-# LANGUAGE ScopedTypeVariables #-}
-- | user handling
module Handler.User where

import Import
import Yesod.MangoPay
import Yesod.MangoPay.Util
import Web.MangoPay
import           Yesod.Form.Jquery
import Control.Monad (join, liftM)

-- | get the form to edit any type of user
getUserR :: AnyUserID -> Handler Html
getUserR uid=do
  eu <- runYesodMPTToken $ getUser uid
  case eu of
    (Left nu)->do
      let muser=Just nu
      (widget, enctype) <- generateFormPost $ naturalUserForm muser
      defaultLayout $ do
            setTitleI MsgTitleNUser
            let (formMethod, msgFormTitle) = formVariables $ Left muser
            $(widgetFile "nuser")
    (Right lu)->do
      let muser=Just lu
      (widget, enctype) <- generateFormPost $ legalUserForm muser
      defaultLayout $ do
            let (formMethod, msgFormTitle) = formVariables $ Right muser
            setTitleI MsgTitleLUser
            $(widgetFile "luser")

-- | get a natural user form
getNUserR :: Handler Html
getNUserR =do
  (muser,widget,enctype)<- userGet naturalUserForm fetchNaturalUser
  defaultLayout $ do
        let (formMethod, msgFormTitle) = formVariables $ Left muser
        setTitleI MsgTitleNUser
        $(widgetFile "nuser")

-- | post a natural user form
postNUserR :: Handler Html
postNUserR = do
  (muser,widget,enctype)<- userPost naturalUserForm createNaturalUser
  defaultLayout $ do
        let (formMethod, msgFormTitle) = formVariables $ Left muser
        setTitleI MsgTitleNUser
        $(widgetFile "nuser")

-- | put a natural user form, to modify an existing natural user
putNUserR :: Handler Html
putNUserR = do
  (muser,widget,enctype)<- userPost naturalUserForm modifyNaturalUser
  defaultLayout $ do
        setTitleI MsgTitleNUser
        let (formMethod, msgFormTitle) = formVariables $ Left muser
        $(widgetFile "nuser")

-- | get a legal user form
getLUserR :: Handler Html
getLUserR = do
  (muser,widget,enctype)<- userGet legalUserForm fetchLegalUser
  defaultLayout $ do
        let (formMethod, msgFormTitle) = formVariables $ Right muser
        setTitleI MsgTitleLUser
        $(widgetFile "luser")

-- | post a legal user form
postLUserR :: Handler Html
postLUserR = do
  (muser,widget,enctype)<- userPost legalUserForm createLegalUser
  defaultLayout $ do
        let (formMethod, msgFormTitle) = formVariables $ Right muser
        setTitleI MsgTitleLUser
        $(widgetFile "luser")


-- | post a legal user form
putLUserR :: Handler Html
putLUserR = do
  (muser,widget,enctype)<- userPost legalUserForm modifyLegalUser
  defaultLayout $ do
        let (formMethod, msgFormTitle) = formVariables $ Right muser
        setTitleI MsgTitleLUser
        $(widgetFile "luser")

-- | common code for retrieval and form building
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

-- | common code for storing, retrieving update and form building
userPost :: HtmlForm a
                 -> (a -> AccessToken -> MangoPayT Handler a)
                 -> Handler (Maybe a, Widget, Enctype)
userPost form store = do
    ((result, _), _) <- runFormPost $ form Nothing
    muser<-case result of
              FormSuccess u -> do
                catchMP (do
                  user<-runYesodMPTToken $ store u
                  setMessageI MsgUserDone
                  return (Just user)
                  )
                  (\e->do
                    setMessage $ toHtml $ show e
                    return (Just u)
                  )
              _ -> do
                setMessageI MsgErrorData
                return Nothing
    (widget, enctype) <- generateFormPost $ form muser
    return (muser,widget,enctype)


-- | form for natural user
naturalUserForm ::  HtmlForm NaturalUser
naturalUserForm muser= renderDivs $ NaturalUser
    <$> aopt hiddenField "" (uId <$> muser)
    <*> pure (join $ uCreationDate <$> muser)
    <*> areq textField (localizedFS MsgUserEmail) (uEmail <$> muser)
    <*> areq textField (localizedFS MsgUserFirst) (uFirstName <$> muser)
    <*> areq textField (localizedFS MsgUserLast) (uLastName <$> muser)
    <*> aopt textField (localizedFS MsgUserAddress) (uAddress <$> muser)
    <*> (day2Posix <$> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) (localizedFS MsgUserBirthday) (posix2Day <$> uBirthday <$> muser))
    <*> areq countryField (localizedFS MsgUserNationality)  (uNationality <$> muser)
    <*> areq countryField (localizedFS MsgUserCountry) (uCountryOfResidence <$> muser)
    <*> aopt textField (localizedFS MsgUserOccupation) (uOccupation <$> muser)
    <*> aopt (selectFieldList ranges) (localizedFS MsgUserIncome) (uIncomeRange <$> muser)
    <*> aopt textField (localizedFS MsgUserCustomData) (uTag <$> muser)
    <*> aopt textField (disabled $ localizedFS MsgUserProofIdentity) (uProofOfIdentity <$> muser) -- value comes from Documents uploaded
    <*> aopt textField (disabled $ localizedFS MsgUserProofAddress) (uProofOfAddress <$> muser) -- value comes from Documents uploaded
  where

-- | form for legal user
legalUserForm :: HtmlForm LegalUser
legalUserForm muser= renderDivs $ LegalUser
    <$> aopt hiddenField "" (lId <$> muser)
    <*> pure (join $ lCreationDate <$> muser)
    <*> areq textField (localizedFS MsgUserEmail) (lEmail <$> muser)
    <*> areq textField (localizedFS MsgUserName) (lName <$> muser)
    <*> areq (selectFieldList ranges) (localizedFS MsgUserPersonType) (lLegalPersonType <$> muser)
    <*> aopt textField (localizedFS MsgUserHQAddress) (lHeadquartersAddress <$> muser)
    <*> areq textField (localizedFS MsgUserRepFirst) (lLegalRepresentativeFirstName <$> muser)
    <*> areq textField (localizedFS MsgUserRepLast) (lLegalRepresentativeLastName <$> muser)
    <*> aopt textField (localizedFS MsgUserRepAddress) (lLegalRepresentativeAddress <$> muser)
    <*> aopt textField (localizedFS MsgUserRepEmail) (lLegalRepresentativeEmail <$> muser)
    <*> (day2Posix <$> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) (localizedFS MsgUserRepBirthday) (posix2Day <$> lLegalRepresentativeBirthday <$> muser))
    <*> areq countryField (localizedFS MsgUserRepNationality) (lLegalRepresentativeNationality <$> muser)
    <*> areq countryField (localizedFS MsgUserRepCountry) (lLegalRepresentativeCountryOfResidence <$> muser)
    <*> pure Nothing  -- value comes from Documents uploaded (I think)
    <*> aopt textField (localizedFS MsgUserCustomData) (lTag <$> muser)
    <*> pure Nothing  -- value comes from Documents uploaded
    <*> pure Nothing -- value comes from Documents uploaded


formVariables :: Either (Maybe NaturalUser) (Maybe LegalUser) -> (Text, AppMessage)
formVariables = either (\n -> process $ mid uId n) (\l -> process $ mid lId l)
  where mid getId mu = do
              u  <- mu
              getId u
        process = maybe ("", MsgUserCreate) (\i -> ("?_method=PUT", MsgUserModify $ i))
