{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Value where

import CssParser.Ident
import CssParser.Norm ( Norm(..) )
import CssParser.Parser.Monad ( reorderErr )
import CssParser.Rule.Value
import CssParser.Rule.Type ( CssType, CssLeafType, AtomicCssType )
import CssParser.Rule.TypedNum ( TypedNum(..), RawNum(..), PropValType (Mm, K), mkRawNum )
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import Data.Text qualified as T

instance Norm NthFormula where
  normalize = \case
    NthExpr e -> NthExpr . normNeg . stripParens $ reorderErr e
    o -> o

instance Arbitrary NthFormula where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

deriving via (GenericArbitrary CssType) instance Arbitrary CssType
deriving via (GenericArbitrary CssLeafType) instance Arbitrary CssLeafType
deriving via (GenericArbitrary AtomicCssType) instance Arbitrary AtomicCssType

data Anum
  = IntAnum Int
  | PositiveIntAnum Word
  | FloatAnum Int Word
  | EAnum Int Int
  | NoWholeAnum Word
  | EAnum2 Int Word Int
  deriving (Eq, Generic)

instance Show Anum where
  show = \case
    IntAnum x -> show x
    PositiveIntAnum x -> "+" <> show x
    FloatAnum x y -> show x <> "." <> show y
    EAnum x y -> show x <> "e" <> show y
    EAnum2 x y z -> show x <> "." <> show y <> "e" <> show z
    NoWholeAnum x -> "." <> show x

deriving via (GenericArbitrary Anum) instance Arbitrary Anum

deriving via (GenericArbitrary Important) instance Arbitrary Important

instance Arbitrary RawNum where
  arbitrary = mkRawNum . show <$> (arbitrary :: Gen Anum)
  shrink _ = []

instance Arbitrary HexColor where
  arbitrary = HC . pack <$> vectorOf 6 arbitraryHex
  shrink (HC x)
    | T.length x == 6 = [HC $ T.take 3 x]
    | otherwise = []


instance Arbitrary Unsigned where
  arbitrary = Unsigned . RawNum . pack <$> listOf1 (elements [ '0' .. '9' ])
  shrink = genericShrink
instance Arbitrary Url where
  arbitrary = oneof
    [ pure $ Url "https://ooo.com/aoeu/style.css"
    , pure $ UnquotedUrl "https://ooo.com:443/aoeu/style.css?y=3&x=ok#eoeu"
    , pure $ UnquotedUrl "./file.css"
    , pure $ UnquotedUrl "/style.css"
    , pure $ UnquotedUrl "/../style.css"
    ]

deriving via Ident instance Arbitrary LiteralString
deriving via (GenericArbitrary Ratio) instance Arbitrary Ratio

instance Norm PropVals where
  normalize (PropVals l i) = PropVals (mapInitNe embraceBinOp embraceDivOp l) i

instance Arbitrary PropVals where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

mapInit :: (a -> a) -> (a -> a) -> [a] -> [a]
mapInit _ _ [] = []
mapInit _ g [a] = [g a]
mapInit f g (h:nh:t) = f h : mapInit f g (nh : t)

mapInitNe :: (a -> a) -> (a -> a) ->  NonEmpty a -> NonEmpty a
mapInitNe f g = \case
  h :| [] -> g h :| []
  h :| l -> f h :| mapInit f g l

embraceBinOp :: PropVal -> PropVal
embraceBinOp = \case
  CalcFun bop@BinOpCe {} -> CalcFun (CalcCe NoFn (CalcExprList $ bop :| []))
  o -> o

embraceDivOp :: PropVal -> PropVal
embraceDivOp = \case
  CalcFun bop@(BinOpCe _ DivCe _) -> CalcFun (CalcCe NoFn (CalcExprList $ bop :| []))
  o -> o

stripImportant :: PropVals -> PropVals
stripImportant = \case
  PropVals l (Just _) -> PropVals l Nothing
  o -> o

instance Norm PropValsList where
  normalize (PropValsList l) = PropValsList (stripImportant <$> l)

instance Arbitrary PropValsList where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

deriving via (GenericArbitrary PropValType) instance Arbitrary PropValType

deriving via (GenericArbitrary CalcFns) instance Arbitrary CalcFns
deriving via (GenericArbitrary CalcOp) instance Arbitrary CalcOp

isD :: Char -> Bool
isD x = isDigit x || x == '.'

normNeg :: CalcExpr -> CalcExpr
normNeg = \case
  o@(CalcNeg (ValCe (TypedNum (RawNum rn) pt))) ->
     case T.uncons rn of
      Just ('-', absRn) -> ValCe (TypedNum (RawNum absRn) pt)
      Just ('+', absRn) ->
        case T.uncons absRn of
          Just (fc, _)
            | isD fc -> ValCe (TypedNum (RawNum (T.cons '-' absRn)) pt)
            | otherwise -> ValCe (TypedNum (RawNum absRn) pt)
          Nothing -> o
      Just (fc, _)
        | isD fc -> ValCe (TypedNum (RawNum (T.cons '-' rn)) pt)
        | otherwise -> o
      _ -> o
  CalcNeg (CalcNeg x) -> normNeg x
  CalcNeg x -> CalcNeg $ normNeg x
  BinOpCe l op r -> BinOpCe (normNeg l) op (normNeg r)
  CalcCe f (CalcExprList l) -> CalcCe f . CalcExprList $ fmap normNeg l
  o -> o

deriving via (GenericArbitrary CalcExpr) instance Arbitrary CalcExpr
deriving via (GenericArbitrary TypedNum) instance Arbitrary TypedNum
deriving via (GenericArbitrary CalcExprList) instance Arbitrary CalcExprList

rightMost :: PropVal -> PropVal
rightMost = \case
  Div _ y -> rightMost y
  o -> o

instance Norm CalcExpr where
  normalize = normNeg

instance Norm PropVal where
  normalize = \case
    Div x y -> Div (rightMost y) (normalize x)
    AppFunEnum f (PropValsList (a :| [])) -> AppFun f a
    CalcFun ce@CalcCe {} -> CalcFun . normNeg . stripParens $ reorderErr ce
    CalcFun ce ->
      CalcFun . CalcCe CalcFn . CalcExprList . (:| []) . normNeg . stripParens $ reorderErr ce
    o -> o

instance Arbitrary PropVal where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink

deriving via (GenericArbitrary CommaSeparatedList) instance Arbitrary CommaSeparatedList

instance Norm AttrType where
  normalize = \case
    UnitAt K -> UnitAt Mm
    o -> o

instance Arbitrary AttrType where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

unPatternLetter :: Gen Char
unPatternLetter = elements ( '?' : ['0' .. '9' ] <> ['a' .. 'f' ])

unPattern :: Gen Text
unPattern = do
  s <- unPatternLetter
  b <- sublistOf =<< vectorOf 3 unPatternLetter
  f <- maybeToList <$> elements [  Nothing, Just '0' ]
  p <- maybeToList <$> elements [  Nothing, Just '1' ]
  pure (pack $ p ++ f ++ b ++ [s])

unDoublePattern :: Gen Text
unDoublePattern = liftA2 (\a b -> a <> "-" <> b) unPattern unPattern

instance Arbitrary UnicodeRange where
  arbitrary = UnicodeRange <$> oneof [unPattern, unDoublePattern]
  shrink (UnicodeRange ur) =
    case T.dropEnd 1 ur of
      ur'->
        case T.unsnoc ur' of
          Just (ur'', '-') -> shrink (UnicodeRange ur'')
          Just (_, _) -> [UnicodeRange ur']
          Nothing -> []
