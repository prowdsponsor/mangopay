-- | documents
module Handler.Doc where

import Import
import Yesod.Core.Types
import Web.MangoPay
import Yesod.MangoPay
import Data.Maybe (fromJust)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Binary (sinkLbs)
import Data.ByteString.Lazy (toStrict)

-- | get the upload form
getDocR :: AnyUserID -> Handler Html
getDocR uid= do
  (widget, enctype) <- generateFormPost uploadForm
  defaultLayout $ do
        aDomId <- newIdent
        setTitle "Upload a document"
        $(widgetFile "docupload")

-- | upload doc and file and show result
postDocR :: AnyUserID -> Handler Html
postDocR uid=do
   ((result, widget), enctype) <- runFormPost uploadForm
   case result of
     FormSuccess (DocUpload fi tag typ)->do
        let doc=Document Nothing Nothing tag typ (Just CREATED) Nothing Nothing
        docWritten0<-runYesodMPTToken $ storeDocument uid doc
        bs<-liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
        runYesodMPTToken $ storePage uid (fromJust $ dId docWritten0) $ toStrict bs
        -- setting to validated causes internal server error...
        docWritten<-runYesodMPTToken $ storeDocument uid (docWritten0{dStatus=Just VALIDATION_ASKED})
        defaultLayout $ do
          aDomId <- newIdent
          setTitle "Document uploaded"
          $(widgetFile "doc")
     _ -> do
            setMessage "Something went wrong"
            redirect $ DocR uid

-- | the upload data type
data DocUpload=DocUpload FileInfo (Maybe Text) DocumentType

-- | the upload form
uploadForm :: Html -> MForm Handler (FormResult DocUpload, Widget)
uploadForm= renderDivs $ DocUpload
  <$> fileAFormReq (fs MsgDocFile)
  <*> aopt textField (fs MsgDocCustomData) Nothing
  <*> areq (selectFieldList ranges) (fs MsgDocType) Nothing
  -- <*> pure (Just CREATED) -- aopt (selectFieldList ranges) (fs MsgDocStatus) Nothing
  