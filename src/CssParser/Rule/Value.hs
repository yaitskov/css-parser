module CssParser.Rule.Value where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Show
import Data.Text qualified as C8

newtype Unsigned = Unsigned Integer
  deriving newtype (Eq, Show, Ord, Num, Enum, Real, Read, Integral, CssShow)
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

data Url
  = Url { unUrl :: Text }
  | UnquotedUrl { unUrl :: Text }
  deriving (Show, Eq, Ord, Generic)
instance CssShow Url where
  toCssText = \case
    Url u -> "url(" <> encodeStringLiteral u <> ")"
    UnquotedUrl u -> "url(" <> fromStrict u <> ")"

data Source = UrlSource Url | StrSource Text
  deriving (Show, Eq, Generic)

instance CssShow Source where
  toCssText = \case
    UrlSource u -> toCssText u
    StrSource t -> encodeStringLiteral t

data PropValType
  = Cap
  | Ch
  | Cm
  | Cqb
  | Cqh
  | Cqi
  | Cqmax
  | Cqmin
  | Cqw
  | Deg
  | Dpi
  | Dvb
  | Dvh
  | Dvi
  | Dvmax
  | Dvmin
  | Em
  | Ex
  | Grad
  | Ic
  | In
  | Lh
  | Lvb
  | Lvh
  | Lvi
  | Lvmax
  | Lvmin
  | Mm
  | Ms
  | Pc
  | Pt
  | Percent
  | Px
  | Q
  | Rad
  | Rcap
  | Rch
  | Rem
  | Rex
  | Ric
  | Rlh
  | Second
  | Svb
  | Svh
  | Svi
  | Svmax
  | Svmin
  | Turn
  | Vb
  | Vh
  | Vi
  | Vmax
  | Vmin
  | Vw
  | K
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance CssShow PropValType where
  toCssText = \case
    Cap -> "cap"
    Ch -> "ch"
    Cm -> "cm"
    Cqb -> "cqb"
    Cqh -> "cqh"
    Cqi -> "cqi"
    Cqmax -> "cqmax"
    Cqmin -> "cqmin"
    Cqw -> "cqw"
    Deg -> "deg"
    Dpi -> "dpi"
    Dvb -> "dvb"
    Dvh -> "dvh"
    Dvi -> "dvi"
    Dvmax -> "dvmax"
    Dvmin -> "dvmin"
    Em -> "em"
    Ex -> "ex"
    Grad -> "grad"
    Ic -> "ic"
    In -> "in"
    Lh -> "lh"
    Lvb -> "lvb"
    Lvh -> "lvh"
    Lvi -> "lvi"
    Lvmax -> "lvmax"
    Lvmin -> "lvmin"
    Mm -> "mm"
    Ms -> "ms"
    Pc -> "pc"
    Pt -> "pt"
    Percent -> "%"
    Px -> "px"
    Q -> "q"
    Rad -> "rad"
    Rcap -> "rcap"
    Rch -> "rch"
    Rem -> "rem"
    Rex -> "rex"
    Ric -> "ric"
    Rlh -> "rlh"
    Second -> "s"
    Svb -> "svb"
    Svh -> "svh"
    Svi -> "svi"
    Svmax -> "svmax"
    Svmin -> "svmin"
    Turn -> "turn"
    Vb -> "vb"
    Vh -> "vh"
    Vi -> "vi"
    Vmax -> "vmax"
    Vmin -> "vmin"
    Vw -> "vw"
    K -> ""

newtype HexColor = HC Text deriving (Eq, Ord, Show, Generic)

instance CssShow HexColor where
  toCssText (HC s) = "#" <> fromStrict s

propRef :: PropertyName -> PropVal
propRef = \case
  PropertyName i -> IdentRef i
  VarProp v -> VarRef v

newtype RawNum = RawNum Text deriving newtype (Eq, Ord, Show) deriving (Generic)

mkRawNum :: String -> RawNum
mkRawNum = RawNum . C8.pack

instance CssShow RawNum where
  toCssText (RawNum x) = fromStrict x

data PropVal
  = IntVal RawNum PropValType
  | RatioVal Ratio
  | IdentRef Ident
  | VarRef Var
  | UrlVal Url
  | StrVal Text
  | AppFun PropertyName PropVals
  | Div PropertyName PropertyName
  | HexColor HexColor
  deriving (Eq, Ord, Show, Generic)

newtype LiteralString = LiteralString Text deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)

instance CssShow LiteralString where
  toCssText (LiteralString s) = encodeStringLiteral s

instance CssShow PropVal where
  toCssText = \case
    IntVal i pvt -> toCssText i <> toCssText pvt
    RatioVal rv -> toCssText rv
    VarRef v -> toCssText v
    IdentRef i -> toCssText i
    UrlVal u -> toCssText u
    StrVal s -> encodeStringLiteral s
    HexColor c -> toCssText c
    Div a b -> toCssText a <> " / " <> toCssText b
    AppFun fn args -> toCssText fn <> "(" <> toCssText args <> ")"

newtype PropVals = PropVals (NonEmpty PropVal) deriving (Show, Eq, Ord, Generic)

instance CssShow PropVals where
  toCssText (PropVals ne) =
    unwords . fmap toCssText $ toList ne

instance ShowSpaceBetween PropVals PropVals where
  cssSpace _ _ = ", "
