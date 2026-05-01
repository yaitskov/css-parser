-- | Functions for Rule types helping with Happy gramma disambiguation
module CssParser.FixRule where

import CssParser.Ident
    ( Ident, Namespace(Namespace, NoBar), TagName(TagName, NoTag) )
import CssParser.Prelude
import CssParser.Rule


tagSelectorOnly :: Ident -> TagSelector
tagSelectorOnly tn = TagSelector NoBar (TagName tn) [] Nothing []

setTag :: Ident -> TagSelector -> TagSelector
setTag tn ts = ts { tagName = TagName tn }

setHash :: Hash -> TagSelector -> TagSelector
setHash h ts = ts { tagId = Just h }

selectorByTag :: Ident -> Selector
selectorByTag tn = Selector Nothing (tagSelectorOnly tn) []

tagNameRule :: Ident -> [CssRuleBodyItem] -> CssRule
tagNameRule tn = CssRule (pure $ selectorByTag tn)

tagAndAttrRule :: Ident -> Attr -> [CssRuleBodyItem] -> CssRule
tagAndAttrRule tn atr body =
  prependAttr atr (CssRule (pure $ selectorByTag tn) body)

prependIdent :: MonadFail m => Ident -> TagRelation -> Selector -> m Selector
prependIdent tn tr = \case
  Selector Nothing fts ots ->
    pure $ Selector Nothing (tagSelectorOnly tn) ((tr, fts) : ots)
  ndsnd@Selector {} ->
    fail $ "Head tag relation is already set: " <> show tn <> ", " <> show tr <> ", " <> show ndsnd
  PeSelector Nothing fts ots pe ->
    pure $ PeSelector Nothing (tagSelectorOnly tn) ((tr, fts) : ots) pe
  ndsnd@PeSelector {} ->
    fail $ "Head tag relation is already set: " <> show tn <> ", " <> show tr <> ", " <> show ndsnd
  PeSelectorOnly pe ->
    pure $ PeSelector Nothing (tagSelectorOnly tn) [] pe

nullTagSelector :: TagSelector
nullTagSelector = TagSelector NoBar NoTag [] Nothing []

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

addClass :: Class -> TagSelector -> TagSelector
addClass c ts  = ts { tagClasses = c : ts.tagClasses }

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
           , tagClasses = AtomicClass c : ts.tagClasses
           }
      _ -> ts

prependIdentAttrSelector :: MonadFail m => Ident -> Attr -> TagRelation -> CssRule -> m CssRule
prependIdentAttrSelector tn atr tr cr = prependAttr atr <$> prependIdentToRule tn tr cr

prependIdentToRule :: MonadFail m => Ident -> TagRelation -> CssRule -> m CssRule
prependIdentToRule tn tr = mapCssRuleM go
  where
    go (fts :| ots) body = do
       fts' <- prependIdent tn tr fts
       pure $ CssRule (fts' :| ots) body

addAttr :: Attr -> TagSelector -> TagSelector
addAttr a ts = ts { tagAttrs = a : ts.tagAttrs }

prependAttr :: Attr -> CssRule -> CssRule
prependAttr a = updateTopTagSelector (addAttr a)

setTopTagName :: Ident -> CssRule -> CssRule
setTopTagName tn = updateTopTagSelector go
  where
    go ts = ts { tagName = TagName tn }
