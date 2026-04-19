module CssParser.File where

import CssParser.At
import CssParser.At.Import
import CssParser.Prelude
import CssParser.Rule
import CssParser.Show
import Data.Maybe (maybeToList)

data CssFile
  = CssFile
    { charset :: Maybe Charset
    , imports :: [ Import ]
    , rules :: NonEmpty CssRule
    }
  deriving (Show, Eq, Generic)

instance CssShow CssFile where
  toCssText cf = unlines $
    maybeToList (toCssText <$> cf.charset) <>
    (toCssText <$> cf.imports) <>
    (toCssText <$> toList cf.rules)
