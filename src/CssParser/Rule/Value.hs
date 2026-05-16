{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Rule.Value
  ( module CssParser.Rule.Value
  , reorder
  ) where

import CssParser.Ident ( Ident, Var, PropertyName(..), AttrName )
import CssParser.Prelude
import CssParser.Rule.Type ( CssType )
import CssParser.Rule.TypedNum
    ( TypedNum, RawNum(..), PropValType, mkRawNum )
import CssParser.Show
    ( CssShow(..), ShowSpaceBetween(..), encodeStringLiteral, mayCss )
import Expression.Reorder
    ( reorder,
      SyntaxTree(..),
      Validation(..),
      Assoc(AssocLeft),
      Fixity(Fixity),
      Node(NodeLeaf, NodeInfix, NodePrefix) )

newtype Unsigned = Unsigned RawNum
  deriving newtype (Eq, Show, Ord, CssShow)
  deriving (Generic)

data Ratio = Ratio Unsigned Unsigned deriving (Eq, Show, Ord, Generic)

instance CssShow Ratio where
  toCssText (Ratio a b) = toCssText a <> "/" <> toCssText b

readRatio :: String -> Either String Ratio
readRatio s =
  case span (/= '/') s of
    ([], _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    ("/", _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    (_, []) -> Left $ "No divisor in Ratio [" <> s <> "]"
    (_, "/") -> Left $ "No divisor in Ratio [" <> s <> "]"
    (divisibleStr, '/':divisorStr) ->
      Right $ Ratio (Unsigned (mkRawNum  divisibleStr)) (Unsigned (mkRawNum divisorStr))
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
  deriving (Show, Ord, Eq, Generic)

instance CssShow Source where
  toCssText = \case
    UrlSource u -> toCssText u
    StrSource t -> encodeStringLiteral t


newtype HexColor = HC Text deriving (Eq, Ord, Show, Generic)

instance CssShow HexColor where
  toCssText (HC s) = "#" <> fromStrict s

propRef :: PropertyName -> PropVal
propRef = \case
  PropertyName i -> IdentRef i
  VarProp v -> VarRef v


data CalcOp = PlusCe | MinusCe | DivCe | ProdCe
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance CssShow CalcOp where
  toCssText = \case
    PlusCe -> " + "
    MinusCe -> " - "
    DivCe -> " / "
    ProdCe -> " * "

data CalcFns
  = CalcFn
  | MinFn
  | NoFn
  | MaxFn
  | ClampFn
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance CssShow CalcFns where
  toCssText = \case
    CalcFn -> "calc"
    MinFn -> "min"
    NoFn -> ""
    MaxFn -> "max"
    ClampFn -> "clamp"

data NthFormula
  = NthEven Bool
  | NthExpr CalcExpr
  deriving (Eq, Ord, Show, Generic)

instance CssShow NthFormula where
  toCssText = \case
    NthEven True -> "even"
    NthEven False -> "odd"
    NthExpr e -> toCssText e

data CalcExpr
  = BinOpCe CalcExpr CalcOp CalcExpr
  | ValCe TypedNum
  | VarCe PropertyName
  | AppCe PropertyName PropValsList
  | CalcCe CalcFns CalcExprList
  | CalcNeg CalcExpr
  deriving (Eq, Ord, Show, Generic)

instance HasParens CalcExpr where
  stripParens = \case
    BinOpCe l op r -> BinOpCe (stripParens l) op (stripParens r)
    CalcCe f (CalcExprList l) -> CalcCe f . CalcExprList $ fmap stripParens l
    CalcNeg (CalcCe NoFn (CalcExprList (x :| []))) -> stripParens $ CalcNeg x
    CalcNeg o -> CalcNeg $ stripParens o
    o@VarCe {} -> o
    o@ValCe {} -> o
    o@AppCe {} -> o

newtype CalcExprList = CalcExprList (NonEmpty CalcExpr)
  deriving newtype (Eq, Ord)
  deriving (Show, Generic)

instance CssShow CalcExprList where
  toCssText (CalcExprList cs) = intercalate ", " (toCssText <$> toList cs)

class HasParens a where
  stripParens :: a -> a

class HasFixity x where
  fixityOf :: x -> Fixity

instance HasFixity CalcOp where
  fixityOf = \case
    PlusCe -> Fixity AssocLeft 1
    MinusCe -> Fixity AssocLeft 2
    ProdCe -> Fixity AssocLeft 3
    DivCe -> Fixity AssocLeft 4

instance Semigroup a => Monad (Validation a) where
  Failure f >>= _ = Failure f
  Success a >>= f = f a

-- happy builtin capabilites for operator priority is pretty limited
-- %left and %right are just ignored for the gramma
-- AST tree is alway represented as a list always spanning to the right
instance SyntaxTree CalcExpr String where
  reorderChildren = \case
    BinOpCe l op r -> BinOpCe <$> reorder l <*> pure op <*> reorder r
    CalcCe x (CalcExprList args) -> CalcCe x . CalcExprList <$> mapM reorder args
    CalcNeg x -> CalcNeg <$> reorder x
    o -> pure o
  structureOf = \case
    BinOpCe l op r -> NodeInfix (fixityOf op) l r (`BinOpCe` op)
    CalcNeg x -> NodePrefix 5 x CalcNeg
    _ -> NodeLeaf
  makeError err _ = show err

instance CssShow CalcExpr where
  toCssText = \case
    BinOpCe a op b -> toCssText a <> toCssText op <> toCssText b
    ValCe v -> toCssText v
    VarCe v -> toCssText v
    AppCe f a -> toCssText f <> "(" <> toCssText a <> ")"
    CalcCe f a -> toCssText f <> "(" <> toCssText a <> ")"
    CalcNeg x@CalcNeg {} -> "-(" <> toCssText x <> ")"
    CalcNeg x@BinOpCe {} -> "-(" <> toCssText x <> ")"
    CalcNeg x@(VarCe _) -> "- " <> toCssText x
    CalcNeg x@(AppCe _ _) -> "- " <> toCssText x
    CalcNeg x -> "-" <> toCssText x

data AttrType
  = CssTypeAt CssType
  | UnitAt PropValType
  | RawString
  deriving (Show, Eq, Ord, Generic)

instance CssShow AttrType where
  toCssText = \case
    RawString -> " raw-string"
    CssTypeAt x -> " type(" <> toCssText x <> ")"
    UnitAt x -> " " <> toCssText x

data PropVal
  = AlphaF Unsigned
  | AppConst PropertyName
  | AppFun PropertyName PropVals
  | AppFunEnum PropertyName PropValsList
  | AttrFun AttrName (Maybe AttrType) (Maybe PropVal)
  | BoolVal Bool
  | CalcFun CalcExpr
  | Div PropVal PropVal
  | DotVal
  | HexColor HexColor
  | IdentRef Ident
  | IntVal TypedNum
  | RatioVal Ratio
  | StrVal Text
  | UnicodeRangeVal UnicodeRange
  | UrlVal Url
  | VarRef Var
  | BracketVal Ident
  deriving (Eq, Ord, Show, Generic)

newtype LiteralString = LiteralString Text deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)

instance CssShow LiteralString where
  toCssText (LiteralString s) = encodeStringLiteral s

instance CssShow PropVal where
  toCssText = \case
    IntVal i -> toCssText i
    VarRef v -> toCssText v
    IdentRef i -> toCssText i
    HexColor c -> toCssText c
    StrVal s -> encodeStringLiteral s
    UrlVal u -> toCssText u
    BoolVal bv -> toCssText bv
    RatioVal rv -> toCssText rv
    CalcFun ce -> toCssText ce
    BracketVal ce -> "[" <> toCssText ce <> "]"
    AttrFun an at dv ->
      "attr(" <> toCssText an <> toCssText at <> mayCss (", " <>) dv <> ")"
    Div a b -> toCssText a <> " / " <> toCssText b
    AppFun fn args -> toCssText fn <> "(" <> toCssText args <> ")"
    AppFunEnum fn args -> toCssText fn <> "(" <> toCssText args <> ")"
    AppConst fn -> toCssText fn <> "()"
    UnicodeRangeVal ur -> toCssText ur
    AlphaF o -> "alpha(opacity=" <> toCssText o <> ")"
    DotVal -> "."


data Important = Important deriving (Show, Eq, Ord, Generic)
instance CssShow Important where
  toCssText _  = " !important"

data PropVals = PropVals (NonEmpty PropVal) (Maybe Important) deriving (Show, Eq, Ord, Generic)

instance CssShow PropVals where
  toCssText (PropVals ne mi) =
    unwords $ (toCssText <$> toList ne) <> maybeToList (toCssText <$> mi)

instance ShowSpaceBetween PropVals PropVals where
  cssSpace _ _ = ", "

newtype PropValsList = PropValsList (NonEmpty PropVals) deriving (Show, Eq, Ord, Generic)

instance CssShow PropValsList where
  toCssText (PropValsList l) = toCssText l

mkAppFun :: PropertyName -> NonEmpty PropVals -> PropVal
mkAppFun pn = \case
  (x :| []) -> AppFun pn x
  o -> AppFunEnum pn (PropValsList o)

newtype UnicodeRange = UnicodeRange Text deriving (Show, Eq, Ord, Generic)
instance CssShow UnicodeRange where
  toCssText (UnicodeRange t) = "U+" <> fromStrict t
instance ShowSpaceBetween UnicodeRange UnicodeRange where
  cssSpace _ _ = ", "

newtype CommaSeparatedList
  = CommaSeparatedList (NonEmpty PropVals)
  deriving (Show, Eq, Ord, Generic)

instance ShowSpaceBetween CommaSeparatedList CommaSeparatedList where
  cssSpace _ _ = "; "
type SrcVal = CommaSeparatedList
instance CssShow CommaSeparatedList where
  toCssText (CommaSeparatedList l) = toCssText l
