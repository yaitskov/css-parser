module CssParser.Rule.Type where

import CssParser.Prelude
import CssParser.Show
import Data.Text (pack)

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

typeMap :: HashMap Text AtomicCssType
typeMap = mkDecodingMap

readSyntaxType :: String -> Either String AtomicCssType
readSyntaxType s =
  case smartLookup (pack s) typeMap of
    Just x -> pure x
    Nothing -> Left $ "Unknown sytnax type: " <> s

newtype CssLeafType = AtomicCssType AtomicCssType
  deriving newtype (Eq, Ord, Show, CssShow) deriving (Generic)

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
