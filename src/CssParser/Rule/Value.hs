module CssParser.Rule.Value where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Show

newtype Unsigned = Unsigned Integer
  deriving newtype (Eq, Show, Ord, Num, Enum, Real, Read, Integral)
  deriving (Generic)

data Ratio = Ratio Unsigned Unsigned deriving (Eq, Show, Ord, Generic)

instance CssShow Ratio where
  toCssText (Ratio a b) = numToText a <> "/" <> numToText b

readRatio :: String -> Either String Ratio
readRatio s =
  case span (/= '/') s of
    ([], _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    ("/", _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    (_, []) -> Left $ "No divisor in Ratio [" <> s <> "]"
    (_, "/") -> Left $ "No divisor in Ratio [" <> s <> "]"
    (divisibleStr, '/':divisorStr) ->
      Ratio <$> readEither divisibleStr <*> readEither divisorStr
    (_, _) -> Left $ "No slash in ratio [" <> s <> "]"

newtype Url = Url { unUrl :: Text }  deriving newtype (Show, Eq, Ord) deriving (Generic)
instance CssShow Url where
  toCssText (Url u) = "url(" <> encodeStringLiteral u <> ")"

data Source = UrlSource Url | StrSource Text
  deriving (Show, Eq, Generic)

instance CssShow Source where
  toCssText = \case
    UrlSource u -> toCssText u
    StrSource t -> encodeStringLiteral t

data PropValType
  = Px | Dpi | Percent | K | Em | Mm | Cm | Vh | Vw | Rad | Deg | Grad | Turn
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance CssShow PropValType where
  toCssText = \case
    Px -> "px"
    Dpi -> "dpi"
    Percent -> "%"
    K -> ""
    Em -> "em"
    Mm -> "mm"
    Vw -> "vw"
    Vh -> "vh"
    Cm -> "cm"

    Rad -> "rad"
    Deg -> "deg"
    Grad -> "grad"
    Turn -> "turn"

data PropVal
  = IntVal Unsigned PropValType
  | RatioVal Ratio
  | IdentRef Ident
  | UrlVal Url
  | StrVal Text
  | AppFun Ident PropVals
  deriving (Eq, Ord, Show, Generic)

newtype LiteralString = LiteralString Text deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)

instance CssShow LiteralString where
  toCssText (LiteralString s) = encodeStringLiteral s

instance CssShow PropVal where
  toCssText = \case
    IntVal i pvt -> numToText i <> toCssText pvt
    RatioVal rv -> toCssText rv
    IdentRef i -> toCssText i
    UrlVal u -> toCssText u
    StrVal s -> encodeStringLiteral s
    AppFun fn args -> toCssText fn <> "(" <> toCssText args <> ")"

newtype PropVals = PropVals (NonEmpty PropVal) deriving (Show, Eq, Ord, Generic)

instance CssShow PropVals where
  toCssText (PropVals ne) =
    unwords . fmap toCssText $ toList ne
