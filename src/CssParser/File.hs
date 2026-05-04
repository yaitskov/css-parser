module CssParser.File where

import CssParser.At ( Charset )
import CssParser.At.Import ( Import )
import CssParser.At.Namespace ( Namespace )
import CssParser.Prelude
import CssParser.Rule ( CssRule, SelectorList )
import CssParser.Rule.Show ()
import CssParser.Show ( CssShow(..) )
import CssParser.At.Layer (LayerStmt)

data FileHeader
  = HeaderImport (Import SelectorList)
  | HeaderLayers LayerStmt
  deriving (Show, Eq, Generic)

instance CssShow FileHeader where
  toCssText = \case
    HeaderImport i -> toCssText i
    HeaderLayers ls -> toCssText ls

data CssFile
  = CssFile
    { charset :: Maybe Charset
    , headers :: [ FileHeader ]
    , namespaces :: [ Namespace ]
    , rules :: [ CssRule ]
    }
  deriving (Show, Eq, Generic)

instance CssShow CssFile where
  toCssText cf = unlines $
    maybeToList (toCssText <$> cf.charset) <>
    (toCssText <$> cf.headers) <>
    (toCssText <$> cf.namespaces) <>
    (toCssText <$> cf.rules)
