-- | Functions for Rule types helping with Happy gramma disambiguation
module CssParser.FixRule where

import CssParser.Ident
    ( Ident (Ident),
      Namespace(Namespace, NoBar),
      PropertyName (PropertyName),
      TagName(TagName, NoTag) )
import CssParser.Prelude
import CssParser.Parser.Monad
import CssParser.Rule
import CssParser.Show ( CssShow(toCssText) )
import CssParser.Rule.Value
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import CssParser.Rule.Pseudo (AtomicPseudoClass)

tagSelectorOnly :: Ident -> TagSelector
tagSelectorOnly tn = TagSelector NoBar (TagName tn) []

setTag :: Ident -> TagSelector -> TagSelector
setTag tn ts = ts { tagName = TagName tn }

setHash :: TagSubSelector -> TagSelector -> TagSelector
setHash = addClass

selectorByTag :: Ident -> Selector
selectorByTag tn = Selector Nothing (tagSelectorOnly tn) []

tagNameRule :: Ident -> [CssRuleBodyItem] -> CssRule
tagNameRule tn = CssRule (pure $ selectorByTag tn)

tagAndAttrRule :: Ident -> TagSubSelector -> [CssRuleBodyItem] -> CssRule
tagAndAttrRule tn atr body =
  prependAttr atr (CssRule (pure $ selectorByTag tn) body)

prependIdent :: MonadFail m => Ident -> TagRelation -> Selector -> m Selector
prependIdent tn tr = \case
  ndsnd@(Selector mtr fts ots) ->
    case mtr of
      Nothing ->
        pure $ Selector Nothing (tagSelectorOnly tn) ((tr, fts) : ots)
      Just Descendant ->
        pure $ Selector Nothing (tagSelectorOnly tn) ((tr, fts) : ots)
      Just ftr
        | tr == Descendant ->
          pure $ Selector Nothing (tagSelectorOnly tn) ((ftr, fts) : ots)
        | otherwise ->
          fail $ "Head tag relation is already set: " <> show tn <> ", " <> show tr <> ", " <> show ndsnd
  ndsnd@(PeSelector mtr fts ots pe) ->
    case mtr of
      Nothing ->
        pure $ PeSelector Nothing (tagSelectorOnly tn) ((tr, fts) : ots) pe
      Just Descendant ->
        pure $ PeSelector Nothing (tagSelectorOnly tn) ((tr, fts) : ots) pe
      Just ftr
        | tr == Descendant ->
          pure $ PeSelector Nothing (tagSelectorOnly tn) ((ftr, fts) : ots) pe
        | otherwise ->
          fail $ "Head tag relation is already set: " <> show tn <> ", " <> show tr <> ", " <> show ndsnd
  PeSelectorOnly pe ->
    pure $ PeSelector (Just tr) (tagSelectorOnly tn) [] pe

nullTagSelector :: TagSelector
nullTagSelector = TagSelector NoBar NoTag []

updateFirstTagSelector :: (TagSelector -> TagSelector) -> Selector -> Selector
updateFirstTagSelector f = \case
  Selector ftr fts ots -> Selector ftr (f fts) ots
  PeSelector ftr fts ots pe -> PeSelector ftr (f fts) ots pe
  PeSelectorOnly pe -> PeSelector Nothing (f nullTagSelector) [] pe

upsertHeadTagSelector :: (TagSelector -> TagSelector) -> CssRule -> [CssRuleBodyItem] -> [CssRuleBodyItem]
upsertHeadTagSelector f cr bodyItems =
  CssNestedRule (mergePrecedingTagSelector f cr) : bodyItems

mergePrecedingTagSelector :: (TagSelector -> TagSelector) -> CssRule -> CssRule
mergePrecedingTagSelector f =
  mapCssRule $ \ (fs :| os) body -> CssRule (go fs :| os) body
  where
    go :: Selector -> Selector
    go = \case
      Selector Nothing fts ots ->
        Selector Nothing (f fts) ots
      Selector (Just fTgRel) fts ots ->
        Selector Nothing (f nullTagSelector) ((fTgRel, fts) : ots)
      PeSelector Nothing fts ots pe ->
        PeSelector Nothing (f fts) ots pe
      PeSelector (Just fTgRel) fts ots pe ->
        PeSelector Nothing (f nullTagSelector) ((fTgRel, fts) : ots) pe
      PeSelectorOnly pe ->
        PeSelector Nothing (f nullTagSelector) [] pe

addClass :: TagSubSelector -> TagSelector -> TagSelector
addClass c ts  = ts { tagSubSelectors = c : ts.tagSubSelectors }

newRule :: (TagSelector -> TagSelector) -> [CssRuleBodyItem] -> [CssRuleBodyItem] -> [CssRuleBodyItem]
newRule f body = (CssNestedRule (CssRule (Selector Nothing (f nullTagSelector) [] :| []) body) :)

mkPeSelector :: (TagSelector -> TagSelector) -> PseudeTagSelector -> Selector
mkPeSelector f = PeSelector Nothing (f nullTagSelector) []

newPseude :: (TagSelector -> TagSelector) -> PseudeTagSelector -> [CssRuleBodyItem] -> [CssRuleBodyItem] -> [CssRuleBodyItem]
newPseude f pts body = (CssNestedRule (CssRule (mkPeSelector f pts :| []) body) :)

pushPeSelector :: (TagSelector -> TagSelector) -> PseudeTagSelector -> CssRule -> CssRule
pushPeSelector f pts =  mapCssRule go
 where
   go :: NonEmpty Selector -> [CssRuleBodyItem] -> CssRule
   go sl = CssRule (mkPeSelector f pts <| sl)

mapCssRuleM :: Monad m => (NonEmpty Selector -> [CssRuleBodyItem] -> m CssRule) -> CssRule -> m CssRule
mapCssRuleM f  = \case
  CssRule selList bis -> f selList bis
  mr@MediaRule {} -> pure mr
  lb@LayerBlock {} -> pure lb
  o -> pure o

mapCssRule :: (NonEmpty Selector -> [CssRuleBodyItem] -> CssRule) -> CssRule -> CssRule
mapCssRule f cr = runIdentity $ mapCssRuleM go cr
  where
    go :: NonEmpty Selector -> [CssRuleBodyItem] -> Identity CssRule
    go a b = pure $ f a b

updateTopTagSelector :: (TagSelector -> TagSelector) -> CssRule -> CssRule
updateTopTagSelector tsF =
  mapCssRule $ \ (fs :| os) body -> CssRule (updateFirstTagSelector tsF fs :| os) body

setTsNs :: Ident -> TagSelector -> TagSelector
setTsNs ns ts = ts { tagNs = Namespace ns }

prependSelectorToRule :: Ident -> CssRule -> CssRule
prependSelectorToRule iden =
  mapCssRule $ \ ss body -> CssRule (selectorByTag iden <| ss) body

tagNameIsClass :: Ident -> CssRule -> CssRule
tagNameIsClass tn = updateTopTagSelector go
  where
    go ts = case ts.tagName of
      TagName c ->
        ts { tagName = TagName tn
           , tagSubSelectors = AtomicClass c : ts.tagSubSelectors
           }
      _ -> ts

prependIdentAttrSelector :: MonadFail m => Ident -> TagSubSelector -> TagRelation -> CssRule -> m CssRule
prependIdentAttrSelector tn atr tr cr = prependAttr atr <$> prependIdentToRule tn tr cr

prependIdentToRule :: MonadFail m => Ident -> TagRelation -> CssRule -> m CssRule
prependIdentToRule tn tr = mapCssRuleM go
  where
    go (fts :| ots) body = do
       fts' <- prependIdent tn tr fts
       pure $ CssRule (fts' :| ots) body

addAttr :: TagSubSelector -> TagSelector -> TagSelector
addAttr a ts = ts { tagSubSelectors = a : ts.tagSubSelectors }

prependAttr :: TagSubSelector -> CssRule -> CssRule
prependAttr a = updateTopTagSelector (addAttr a)

setTopTagName :: Ident -> CssRule -> CssRule
setTopTagName tn = updateTopTagSelector go
  where
    go ts = ts { tagName = TagName tn }

pclassToIdent :: AtomicPseudoClass -> Ident
pclassToIdent = Ident . T.drop 1 . L.toStrict . toCssText

pclassToPropVals :: Maybe Important-> AtomicPseudoClass -> PropVals
pclassToPropVals mi pc = PropVals (IdentRef (pclassToIdent pc) :| []) mi

rewritePseudoClassAsDescValue :: Ident -> Maybe Important -> AtomicPseudoClass -> CssRuleBodyItem
rewritePseudoClassAsDescValue pn mi pc =
 CssLeafRule (PropertyName pn) (pclassToPropVals mi pc)

rewritePseudoClassAsPropValsImp :: Ident -> AtomicPseudoClass -> Maybe Important -> NonEmpty PropVals -> CssRuleBodyItem
rewritePseudoClassAsPropValsImp pn pc mi pvl =
  CssEnumLeaf
    (PropertyName pn)
    (PropValsList $ pclassToPropVals mi pc <| pvl)

rewritePseudoClassAsPropVals :: Ident -> AtomicPseudoClass -> NonEmpty PropVals -> CssRuleBodyItem
rewritePseudoClassAsPropVals pn pc (pvs :| pvl) =
  case pvs of
    PropVals pv mi ->
      CssEnumLeaf
        (PropertyName pn)
        (PropValsList $ PropVals (IdentRef (pclassToIdent pc) <| pv) mi :| pvl)

fixNotClass :: PropertyName -> Ident -> Maybe Important -> CssRuleBodyItem
fixNotClass pn nsuf mi =
  CssLeafRule pn  (PropVals (IdentRef ("not-" <> nsuf) :| []) mi)

mkLeaf :: PropertyName -> NonEmpty PropVals -> CssRuleBodyItem
mkLeaf pn = \case
  (x :| []) -> CssLeafRule pn x
  o -> CssEnumLeaf pn (PropValsList o)

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 f = fmap (fmap f)

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
