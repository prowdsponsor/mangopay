{-# LANGUAGE RankNTypes #-}
-- | utility functions for Yesod, that are used in the site but are not really generic enough to be put in the library
module Base.Util where

import Yesod
import Prelude
import Foundation

import Data.Text hiding (map)
import Control.Arrow ((&&&))


-- | localized field
fs :: forall master msg.
            RenderMessage master msg =>
            msg -> FieldSettings master
fs n=FieldSettings (SomeMessage n) Nothing Nothing Nothing []    


-- | show text and identifier for all values of an enum    
ranges :: forall a. (Bounded a, Enum a, Show a) => [(Text, a)]
ranges=map (pack . show &&& id) [minBound..maxBound] 

-- | the type of an html form
type HtmlForm a= Maybe a -> Html -> MForm Handler (FormResult a, Widget)