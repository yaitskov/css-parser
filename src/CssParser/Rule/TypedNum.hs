module CssParser.Rule.TypedNum where


import CssParser.Parser.Monad
import CssParser.Prelude
import CssParser.Show
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as C8

type NumberStr = String

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
  | Fr
  | Grad
  | Hz
  | Ic
  | In
  | KHz
  | Lh
  | Lvb
  | Lvh
  | Lvi
  | Lvmax
  | Lvmin
  | Mm
  | Ms
  | N  -- virtual unit used for pasing nth-child(-2n + 1)
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
    Fr -> "fr"
    Grad -> "grad"
    Hz -> "Hz"
    KHz -> "kHz"
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
    N  -> "n"
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

unitMap :: HM.HashMap Text PropValType
unitMap = mkDecodingMap

newtype RawNum = RawNum Text
  deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)

mkRawNum :: String -> RawNum
mkRawNum = RawNum . C8.pack

instance CssShow RawNum where
  toCssText (RawNum x) = fromStrict x

data TypedNum = TypedNum RawNum PropValType deriving (Show, Ord, Eq, Generic)

instance CssShow TypedNum where
  toCssText (TypedNum n t) = toCssText n <> toCssText t

tryGet :: PropValType -> TypedNum -> P RawNum
tryGet tt tn@(TypedNum v t)
  | t == tt = pure v
  | otherwise = Failed $ "Expected " <> toCssStr tt <> " but " <> toCssStr tn

parseTypedNum :: NumberStr -> P TypedNum
parseTypedNum = go . break isDigit . reverse
  where
    go (rSuf, rNum) =
      let suf = reverse rSuf in
        case smartLookup (C8.pack suf) unitMap of
          Just pvt ->
            pure $ TypedNum (mkRawNum $ reverse rNum) pvt
          Nothing ->
            Failed $ "Unknown number unit: " <> suf <> " in " <> reverse rNum

parseAsUnitType :: Text -> P PropValType
parseAsUnitType ut =
  case smartLookup ut unitMap of
    Just pvt -> pure pvt
    Nothing -> Failed $ "Unkown number unit: " <> C8.unpack ut
