-- | Functions for Rule types helping with Happy gramma disambiguation
module CssParser.FixRule where

import Control.Monad ( foldM )
import CssParser.Descriptor (Descriptor (CustomDescriptor, KnownDescriptor), KnownDescriptor (ResultT))
import CssParser.At.Function ( ConstEntry(..) )
import CssParser.Ident
import CssParser.Prelude
import CssParser.Parser.Monad ( P(Failed) )
import CssParser.Rule
    ( CssRuleBodyItem(CssEnumLeaf, CssLeafRule),
      TagSelector(TagSelector) )
import CssParser.Show ( CssShow(toCssText) )
import CssParser.Rule.Value
import CssParser.Rule.Show ()
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import CssParser.Rule.Pseudo (AtomicPseudoClass)

nullTagSelector :: TagSelector
nullTagSelector = TagSelector NoBar NoTag []

identOnly :: PropertyName -> (Ident -> a) -> P a
identOnly pn f = identOnlyP pn (pure . f)

identOnlyP :: PropertyName -> (Ident -> P a) -> P a
identOnlyP (PropertyName i) f = f i
identOnlyP (VarProp i) _ = Failed $ "Var " <> show i <> " is not expected"

varOrResult :: Descriptor -> NonEmpty PropVals -> P (Either ConstEntry (NonEmpty PropVals))
varOrResult d pvs =
  case d of
    KnownDescriptor ResultT -> pure $ Right pvs
    CustomDescriptor i -> pure . Left $ ConstEntry (Var i) (PropValsList pvs)
    o -> Failed $ "Expected var or result but got " <> unpack (toCssText o)

findResult :: [ Either ConstEntry (NonEmpty PropVals) ] -> P (NonEmpty PropVals, [ConstEntry])
findResult l =
  foldM go (Nothing, []) l >>= \case
    (Nothing, _) -> Failed "Function body do not have result: descriptor"
    (Just r, ces) -> pure (r, reverse ces)
  where
    go b a =
      case (b, a) of
       ((Just r, _), Right secondR) ->
         Failed $ "Multiple result descriptors with values: " <>
           unpack (toCssText r) <> " and " <> unpack (toCssText secondR)
       ((Nothing, ces), Right r) ->
         pure (Just r, ces)
       ((r, ces), Left ce) ->
         pure (r, ce : ces)


specificDescOnlyP :: Descriptor -> Descriptor -> P ()
specificDescOnlyP e g
  | e == g = pure ()
  | otherwise =
    Failed $ "Expected " <> unpack (toCssText e) <> " but got " <> unpack (toCssText g)

pclassToIdent :: AtomicPseudoClass -> Ident
pclassToIdent = Ident . T.drop 1 . L.toStrict . toCssText

prependPropVal :: PropVal -> PropVals -> PropVals
prependPropVal pv (PropVals pvs mi) = PropVals (pv <| pvs) mi

pclassToPropVal :: AtomicPseudoClass -> PropVal
pclassToPropVal pc = IdentRef (pclassToIdent pc)

pclassToPropVals :: Maybe Important-> AtomicPseudoClass -> PropVals
pclassToPropVals mi pc = PropVals (pclassToPropVal pc :| []) mi

mkLeaf :: Descriptor -> NonEmpty PropVals -> CssRuleBodyItem
mkLeaf pn = \case
  (x :| []) -> CssLeafRule pn x
  o -> CssEnumLeaf pn (PropValsList o)

chopOffLeftmostSign :: CalcExpr -> Maybe (CalcOp, CalcExpr)
chopOffLeftmostSign = \case
  BinOpCe a op b -> do
    case chopOffLeftmostSign a of
      Nothing -> Nothing
      Just (lop, a') -> Just (lop, BinOpCe a' op b)
  ValCe (RawNum rn) pt ->
    case T.uncons rn of
      Just ('-', absRn) -> pure (MinusCe, ValCe (RawNum absRn) pt)
      Just ('+', absRn) -> pure (PlusCe, ValCe (RawNum absRn) pt)
      _ -> Nothing
  _ -> Nothing

recoverCalcBinOp :: CalcExpr -> CalcExpr -> P CalcExpr
recoverCalcBinOp fo so =
  case chopOffLeftmostSign so of
    Nothing ->
      fail $ "Expected operator between " <> unpack (toCssText fo) <> " and " <> unpack (toCssText so)
    Just (lop, so') ->
      pure $ BinOpCe fo lop so'
