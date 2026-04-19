module CssParser.File where

import CssParser.Rule
import CssParser.Show
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics (Generic)
import Prelude

newtype CssFile
  = CssFile
    { rules :: NonEmpty CssRule
    }
  deriving newtype (Show, Eq, CssShow)
  deriving (Generic)
