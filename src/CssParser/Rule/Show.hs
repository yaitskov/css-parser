{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Rule.Show where

import CssParser.Prelude

import CssParser.Show
import CssParser.Rule
import CssParser.Ident
import CssParser.Rule.Pseudo
import CssParser.Utils ( encodeIdentifier )


instance CssShow CssRule where
  toCssText = \case
    CssRule sels body ->
      intercalate ", " (toList $ fmap toCssText sels) <> "{" <> toCssText body <> "}"
    MediaRule mql body -> toCssText mql <> " {" <> toCssText body <> "}"
    LayerBlock mbn body ->
      "@layer " <> maybe "" ((<> " ") . toCssText) mbn <> "{" <> toCssText body <> "}"

instance CssShow CssRuleBodyItem where
  toCssText = \case
    CssNestedRule cr -> toCssText cr
    CssLeafRule pn pv -> toCssText pn <> ": " <> toCssText pv <> "; "

instance CssShow TagRelation where
  toCssText = \case
    Descendant -> " "
    Child -> " > "
    NextSibling -> " + "
    GeneralSibling -> " ~ "

instance CssShow AttrOp where
  toCssText = \case
    Exact -> "="
    Include -> "~="
    DashMatch -> "|="
    PrefixMatch -> "^="
    SuffixMatch -> "$="
    SubstringMatch -> "*="

instance CssShow Class where
  toCssText = \case
    AtomicClass (Ident uc) -> cons '.' $ encodeIdentifier uc
    AtomicPseudoClass apc -> toCssText apc
    NotClass nes -> ":not(" <> (intercalate ", " . toList $ fmap toCssText nes) <> ")"
    Lang (Language l) -> ":lang(" <> fromStrict l <> ")"
    NthChild nth -> ":nth-child(" <> toCssText nth <> ")"
    NthLastChild nth -> ":nth-last-child(" <> toCssText nth <> ")"
    NthLastOfType nth -> ":nth-last-of-type(" <> toCssText nth <> ")"
    NthOfType nth -> ":nth-of-type(" <> toCssText nth <> ")"

instance CssShow Attr where
  toCssText (HasAttr name) = "[" <> toCssText name <> "]"
  toCssText (Attr name op val) =
    "[" <> toCssText name <>
    toCssText op <>
    encodeStringLiteral val <>
    "]"

instance CssShow AttrName where
  toCssText (AttrName n (Ident e)) = toCssText n <> encodeIdentifier e

instance CssShow Hash where
  toCssText = cons '#' . encodeIdentifier . unHash

instance CssShow Selector where
  toCssText = \case
    Selector fts tss ->
      fold fts tss
    PeSelector fts tss pe ->
      fold fts tss <> toCssText pe
    PeSelectorOnly pe -> toCssText pe
    where
      fold fts =
        foldl
          (\ s (tr, ts) -> s <> toCssText tr <> toCssText ts)
          (toCssText fts)

instance CssShow TagSelector where
  toCssText ts =
    concat $
    [ toCssText ts.tagNs
    , toCssText ts.tagName
    ]
    <>
    case ts.tagAttrs of
      [] -> [""]
      o -> fmap toCssText o
    <>
    [ maybe "" toCssText ts.tagId
    , concat (toCssText <$> ts.tagClasses)
    {- HLINT ignore "Use concatMap" -}
    ]
