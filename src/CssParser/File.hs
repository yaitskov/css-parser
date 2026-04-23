module CssParser.File where

import CssParser.At ( Charset )
import CssParser.At.Import ( Import )
import CssParser.Prelude
import CssParser.Rule ( CssRule )
import CssParser.Rule.Show ()
import CssParser.Show
import CssParser.At.Layer (LayerStmt)

data FileHeader
  = HeaderImport Import
  | HeaderLayers LayerStmt
  deriving (Show, Eq, Generic)

instance CssShow FileHeader where
  toCssText = \case
    HeaderImport i -> toCssText i
    HeaderLayers ls -> toCssText ls

data CssFile
  = CssFile
    { charset :: Maybe Charset
    , headers :: [ FileHeader  ]
    , rules :: NonEmpty CssRule
    }
  deriving (Show, Eq, Generic)

instance CssShow CssFile where
  toCssText cf = unlines $
    maybeToList (toCssText <$> cf.charset) <>
    (toCssText <$> cf.headers) <>
    (toCssText <$> toList cf.rules)
