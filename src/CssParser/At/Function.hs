{-# LANGUAGE UndecidableInstances #-}
module CssParser.At.Function where

import CssParser.Ident ( Ident, Var )
import CssParser.Rule.Value ( PropVal, PropVals, PropValsList )
import CssParser.Prelude
import CssParser.Show
    ( CssShow(..), Embraced(Embraced), ShowSpaceBetween(..), Csl(Csl) )

data AtomicCssType
  = Angle
  | Color
  | CustomIdent
  | Image
  | Integer
  | Length
  | LengthPercentage
  | Number
  | Percentage
  | Resolution
  | String
  | Time
  | TranformFunction
  | TranformList
  | UrlType
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance CssShow AtomicCssType where
  toCssText = \case
    Angle -> "<angle>"
    Color -> "<color>"
    CustomIdent -> "<custom-ident>"
    Image -> "<image>"
    Integer -> "<integer>"
    Length -> "<length>"
    LengthPercentage -> "<length-percentage>"
    Number -> "<number>"
    Percentage -> "<percentage>"
    Resolution -> "<resolution>"
    String -> "<string>"
    Time -> "<time>"
    TranformFunction -> "<tranform-function>"
    TranformList -> "<tranform-list>"
    UrlType -> "<url>"

data CssLeafType
  = AtomicCssType AtomicCssType
  | IdentCssType Ident
  deriving (Eq, Ord, Show, Generic)

instance CssShow CssLeafType where
  toCssText = \case
    AtomicCssType x -> toCssText x
    IdentCssType x -> toCssText x

data CssType
  = Once CssLeafType
  | AnyCssType
  | CommaSeparated CssLeafType
  | SpaceSeparated CssLeafType
  | OrLeaf CssLeafType CssType
  deriving (Eq, Ord, Show, Generic)

instance CssShow CssType where
  toCssText = \case
    Once a -> toCssText a
    AnyCssType -> "*"
    OrLeaf x t -> toCssText x <> " | " <> toCssText t
    CommaSeparated a -> toCssText a <> "#"
    SpaceSeparated a -> toCssText a <> "+"

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
