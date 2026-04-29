{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Rule.Show where

import CssParser.Ident ( Ident(Ident), AttrName(AttrName) )
import CssParser.MonoPair
import CssParser.Prelude
import CssParser.Rule
import CssParser.Rule.Pseudo ( Language(Language) )
import CssParser.Show
import CssParser.Utils ( encodeIdentifier )

instance CssShow CssRule where
  toCssText = \case
    CssRule sels body ->
      intercalate ", " (toList $ fmap toCssText sels) <> "{" <> toCssText body <> "}"
    MediaRule mql body -> toCssText mql <> " {" <> toCssText body <> "}"
    LayerBlock mbn body ->
      "@layer " <> maybe "" ((<> " ") . toCssText) mbn <> "{" <> toCssText body <> "}"
    Page psl body ->
      toCssText psl <> " {" <> toCssText body <> "}"
    PageMarginBlock pm body ->
      toCssText pm <> " {" <> toCssText body <> "}"
    CounterStyle cn body ->
      "@counter-style " <> toCssText cn <> " {" <> toCssText body <> "}"
    Property pn body ->
      "@property " <> toCssText pn <> " {" <> toCssText body <> "}"
    Keyframes kf -> toCssText kf
    ColorProfile n b ->
      "@color-profile " <> toCssText n <> " {" <> toCssText b <> "}"
    FontFaceBlock ff -> toCssText ff
    FontFeatureValuesBlock ffv -> toCssText ffv
    FontPaletteValuesBlock ffv -> toCssText ffv
    Container cq body ->
      "@container " <> toCssText cq <> " {" <> toCssText body <> "}"
    PositionTry v pl ->
      "@position-try " <> toCssText v <> " {" <> toCssText pl <> "}"
    StartingStyle body ->
      "@starting-style {" <> toCssText body <> "}"
    ViewTransition body ->
      "@view-transition {" <> toCssText body <> "}"
    ScopeBlock range body ->
      "@scope " <> toCssText range <> embrace body
    Supports fq body ->
      "@supports " <> toCssText fq <> embrace body
    UnknownGramma i query body ->
      "@" <> toCssText i <> " " <> maybe "" toCssText query <> embrace body

embrace :: CssShow a => a -> LText
embrace x = " {" <> toCssText x <> "}"

instance ShowSpaceBetween (MonoPair SelectorList) SelectorList  where
  cssSpace _ _ = ") to ("
instance ShowParenthesis (MonoPair SelectorList) SelectorList where
  left _ _ = "("
  right _ _ = ")"
instance ShowSpaceBetween CssRuleBodyItem CssRuleBodyItem where
  cssSpace _ _ = " "

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
instance ShowSpaceBetween Selector Selector where
  cssSpace _ _ = ", "
instance CssShow Class where
  toCssText = \case
    AtomicClass (Ident uc) -> cons '.' $ encodeIdentifier uc
    AtomicPseudoClass apc -> toCssText apc
    NotClass nes -> ":not(" <> toCssText nes <> ")"
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
    Selector frl fts tss ->
      fold frl fts tss
    PeSelector frl fts tss pe ->
      fold frl fts tss <> toCssText pe
    PeSelectorOnly pe -> toCssText pe
    where
      fold frl fts =
        foldl
          (\ s (tr, ts) -> s <> toCssText tr <> toCssText ts)
          (maybe "" toCssText frl <> toCssText fts)

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
