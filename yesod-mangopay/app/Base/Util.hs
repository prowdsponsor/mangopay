{-# LANGUAGE RankNTypes #-}
-- | utility functions for Yesod, that are used in the site but are not really generic enough to be put in the library
module Base.Util where

import Yesod
import Prelude
import Foundation

import Data.Text hiding (map)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Data.Text.Read (decimal)

import Web.MangoPay

import Data.ISO3166_CountryCodes (CountryCode, readableCountryName)

-- | localized field
localizedFS :: forall master msg.
            RenderMessage master msg =>
            msg -> FieldSettings master
localizedFS n=FieldSettings (SomeMessage n) Nothing Nothing Nothing []    

-- | disabled field
disabled :: forall master.
              FieldSettings master -> FieldSettings master
disabled fs= fs{fsAttrs= ("disabled",""):fsAttrs fs}


-- | disable field if the maybe is just (for fields you can set when creating but not when editing)
disabledIfJust :: forall t master.
                    Maybe t -> FieldSettings master -> FieldSettings master
disabledIfJust (Just _)=disabled
disabledIfJust _=id

-- | show text and identifier for all values of an enum    
ranges :: forall a. (Bounded a, Enum a, Show a) => [(Text, a)]
ranges=map (pack . show &&& id) [minBound..maxBound] 

-- | the type of an html form
type HtmlForm a= Maybe a -> Html -> MForm Handler (FormResult a, Widget)

-- | supported currencies
supportedCurrencies :: [Currency]
supportedCurrencies=["EUR","USD","GBP","PLN","CHF"]

-- | extract pagination info from parameters
getPagination :: forall (m :: * -> *).
                   MonadHandler m =>
                   m (Maybe Pagination)
getPagination = do
    -- poor man's parameter handling...
    pg<-liftM (fromMaybe "1") $ lookupGetParam "page"
    let Right (i,_)=decimal pg
    return $ Just $ Pagination i 10
    
-- | previous and next page number
getPaginationNav :: forall a.
                      Maybe Pagination -> PagedList a -> (Maybe Integer, Maybe Integer)
getPaginationNav (Just (Pagination i _)) l=let
    next=if plPageCount l > i
              then Just (i+1)
              else Nothing
    previous=if i>1
              then Just (i-1)
              else Nothing
    in (previous,next)
getPaginationNav _ _= (Nothing,Nothing)             
    
    
-- | country field
countryField :: RenderMessage site FormMessage =>
                  Field (HandlerT site IO) CountryCode
countryField = selectFieldList $ map (pack . readableCountryName &&& id) [minBound..maxBound] 
