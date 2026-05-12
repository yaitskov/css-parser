{-# LANGUAGE UndecidableInstances #-}
module CssParser.At.Function where

import CssParser.Ident ( Var )
import CssParser.Rule.Type
import CssParser.Rule.Value ( PropVal, PropVals, PropValsList )
import CssParser.Prelude
import CssParser.Show
    ( CssShow(..), Embraced(Embraced), ShowSpaceBetween(..), Csl(Csl) )

data FunArg = FunArg
  { argName :: Var
  , argType :: Maybe CssType
  , defaultValue :: Maybe PropVal
  }
  deriving (Eq, Ord, Show, Generic)

instance CssShow FunArg where
  toCssText fa =
    toCssText fa.argName <>
    maybe "" ((" type(" <> ) . (<> ")") . toCssText) fa.argType <>
    maybe "" ((" : " <> ) . toCssText) fa.defaultValue

data ConstEntry = ConstEntry Var PropValsList deriving (Show, Eq, Ord, Generic)
instance ShowSpaceBetween ConstEntry ConstEntry where
  cssSpace _ _ = ""
instance CssShow ConstEntry where
  toCssText (ConstEntry pn pv) =
    toCssText pn <> ": " <>  toCssText pv <> ";"

data Function r
  = Function
  { name :: Var
  , args :: [FunArg]
  , returns :: Maybe CssType
  , localConsts :: [ ConstEntry ]
  , result :: NonEmpty PropVals
  , atRules :: [r]
  } deriving (Eq, Ord, Show, Generic)

instance (ShowSpaceBetween r r, CssShow r) => CssShow (Function r) where
  toCssText p =
    toCssText p.name <> toCssText (Embraced (Csl p.args)) <>
    maybe "" ((" returns type(" <>) . (<> ")"). toCssText) p.returns <>
    "{" <> toCssText p.localConsts <> " result: " <> toCssText p.result <> ";" <>
    toCssText p.atRules <> "}"
