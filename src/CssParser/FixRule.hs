-- | Functions for Rule types helping with Happy gramma disambiguation
module CssParser.FixRule where

import CssParser.Ident
    ( Ident, Namespace(Namespace, NoBar), TagName(TagName, NoTag) )
import CssParser.Prelude
import CssParser.Rule
    ( Attr,
      Class(AtomicClass),
      CssRule(..),
      CssRuleBodyItem,
      Selector(..),
      TagRelation,
      TagSelector(tagAttrs, TagSelector, tagNs, tagName, tagClasses) )

tagSelectorOnly :: Ident -> TagSelector
tagSelectorOnly tn =
  TagSelector NoBar (TagName tn) [] Nothing []

selectorByTag :: Ident -> Selector
selectorByTag tn = Selector (tagSelectorOnly tn) []

tagNameRule :: Ident -> [CssRuleBodyItem] -> CssRule
tagNameRule tn = CssRule (pure $ selectorByTag tn)

tagAndAttrRule :: Ident -> Attr -> [CssRuleBodyItem] -> CssRule
tagAndAttrRule tn atr body =
  prependAttr atr (CssRule (pure $ selectorByTag tn) body)

prependIdent :: Ident -> TagRelation -> Selector -> Selector
prependIdent tn tr = \case
  Selector fts ots -> Selector (tagSelectorOnly tn) ((tr, fts) : ots)
  PeSelector fts ots pe -> PeSelector (tagSelectorOnly tn) ((tr, fts) : ots) pe
  PeSelectorOnly pe -> PeSelector (tagSelectorOnly tn) [] pe

nullTagSelector :: TagSelector
nullTagSelector = TagSelector NoBar NoTag [] Nothing []

updateFirstTagSelector :: (TagSelector -> TagSelector) -> Selector -> Selector
updateFirstTagSelector f = \case
  Selector fts ots -> Selector (f fts) ots
  PeSelector fts ots pe -> PeSelector (f fts) ots pe
  PeSelectorOnly pe -> PeSelector (f nullTagSelector) [] pe

mapCssRule :: (NonEmpty Selector -> [CssRuleBodyItem] -> CssRule) -> CssRule -> CssRule
mapCssRule f  = \case
  CssRule s bis -> f s bis
  mr@MediaRule {} -> mr
  lb@LayerBlock {} -> lb

updateTopTagSelector :: (TagSelector -> TagSelector) -> CssRule -> CssRule
updateTopTagSelector tsF =
  mapCssRule $ \ (fts :| ots) body -> CssRule (updateFirstTagSelector tsF fts :| ots) body

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

prependIdentAttrSelector :: Ident -> Attr -> TagRelation -> CssRule -> CssRule
prependIdentAttrSelector tn atr tr  = prependAttr atr . prependIdentToRule tn tr

prependIdentToRule :: Ident -> TagRelation -> CssRule -> CssRule
prependIdentToRule tn tr =
  mapCssRule $ \ (fts :| ots) body -> CssRule (prependIdent tn tr fts :| ots) body

prependAttr :: Attr -> CssRule -> CssRule
prependAttr atr = updateTopTagSelector go
  where
    go ts = ts { tagAttrs = atr : ts.tagAttrs }

setTopTagName :: Ident -> CssRule -> CssRule
setTopTagName tn = updateTopTagSelector go
  where
    go ts = ts { tagName = TagName tn }
