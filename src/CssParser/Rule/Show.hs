{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Rule.Show where

import CssParser.Ident ( Ident(Ident) )
import CssParser.MonoPair ( MonoPair )
import CssParser.Prelude
import CssParser.Rule
import CssParser.Rule.Pseudo ( Language(Language) )
import CssParser.Show
    ( encodeStringLiteral,
      CssShow(..),
      ShowParenthesis(..),
      ShowSpaceBetween(..) )
import CssParser.Utils ( encodeIdentifier )

instance ShowSpaceBetween CssRule CssRule where
  cssSpace _ _ = "\n"

instance CssShow CssRule where
  toCssText = \case
    CssRule sels body ->
      intercalate ", " (toList $ fmap toCssText sels) <> "{" <> toCssText body <> "}"
    AtRule pb ar ->
      "@" <> toCssText pb <> toCssText ar

instance CssShow AtRule where
  toCssText = \case
    MediaRule mql body ->
      "media " <> toCssText mql <> " {" <> toCssText body <> "}"
    LayerBlock mbn body ->
      "layer " <> maybe "" ((<> " ") . toCssText) mbn <> "{" <> toCssText body <> "}"
    ImportStmt i ->
      "import " <> toCssText i <> ";"
    LayerStmt l ->
      "layer " <> toCssText l <> ";"
    Namespace i s ->
      "namespace " <> maybe "" ((<> " ") . toCssText) i <> toCssText s <> ";"
    CharsetStmt cs ->
      "charset " <> toCssText cs <> ";"
    Page psl body ->
      "page " <> toCssText psl <> " {" <> toCssText body <> "}"
    PageMarginBlock pm body ->
      toCssText pm <> " {" <> toCssText body <> "}"
    CounterStyle cn body ->
      "counter-style " <> toCssText cn <> " {" <> toCssText body <> "}"
    Property pn body ->
      "property " <> toCssText pn <> " {" <> toCssText body <> "}"
    Keyframes kf ->
      "keyframes " <>  toCssText kf
    ColorProfile n b ->
      "color-profile " <> toCssText n <> " {" <> toCssText b <> "}"
    FontFaceBlock ff ->
      "font-face {" <> toCssText ff <> "}"
    FontFeatureValuesBlock ffv -> toCssText ffv
    FontPaletteValuesBlock ffv -> toCssText ffv
    Container cq body ->
      "container " <> toCssText cq <> " {" <> toCssText body <> "}"
    PositionTry v pl ->
      "position-try " <> toCssText v <> " {" <> toCssText pl <> "}"
    StartingStyle body ->
      "starting-style {" <> toCssText body <> "}"
    ViewTransition body ->
      "view-transition {" <> toCssText body <> "}"
    ScopeBlock range body ->
      "scope " <> toCssText range <> embrace body
    Supports fq body ->
      "supports " <> toCssText fq <> embrace body
    FunctionBlock f ->
      "function " <> toCssText f
    UnknownGramma i query body ->
      toCssText i <> " " <> maybe "" toCssText query <> embrace body

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
    CssLeafRule pn pv -> toCssText pn <> toCssText pv <> ";"
    CssEnumLeaf pn pv -> toCssText pn <> toCssText pv <> ";"

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
instance CssShow TagSubSelector where
  toCssText = \case
    AtomicClass (Ident uc) -> cons '.' $ encodeIdentifier uc
    AtomicPseudoClass apc -> toCssText apc
    NotClass nes -> ":not(" <> toCssText nes <> ")"
    Lang (Language l) -> ":lang(" <> fromStrict l <> ")"
    Global nes -> ":global(" <> toCssText nes <> ")"
    Where nes -> ":where(" <> toCssText nes <> ")"
    Is nes -> ":is(" <> toCssText nes <> ")"
    Has nes -> ":has(" <> toCssText nes <> ")"
    NthChild nth -> ":nth-child(" <> toCssText nth <> ")"
    NthLastChild nth -> ":nth-last-child(" <> toCssText nth <> ")"
    NthLastOfType nth -> ":nth-last-of-type(" <> toCssText nth <> ")"
    NthOfType nth -> ":nth-of-type(" <> toCssText nth <> ")"
    ActiveViewTransitionType x -> ":active-view-transition-type" <> toCssText x
    Dir x -> ":dir" <> toCssText x
    Heading x -> ":heading" <> toCssText x
    Host x -> ":host" <> toCssText x
    State x -> ":state" <> toCssText x
    HasAttr name -> "[" <> toCssText name <> "]"
    Attr name op val ->
      "[" <> toCssText name <>
      toCssText op <>
      toCssText val <>
      "]"
    Hash h -> cons '#' $ toCssText h

instance CssShow AtrPat where
  toCssText = \case
    StrAtrPat s cs -> encodeStringLiteral s <> toCssText cs
    IdtAtrPat i cs -> toCssText i <> toCssText cs

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

instance ShowSpaceBetween TagSubSelector TagSubSelector where
  cssSpace _ _ = ""

instance CssShow PseudeTagSelector where
  toCssText pts =
    toCssText pts.ptagName <> toCssText pts.ptagSubs

instance CssShow TagSelector where
  toCssText ts =
    concat $
    [ toCssText ts.tagNs
    , toCssText ts.tagName
    ]
    <> (toCssText <$> ts.tagSubSelectors)
    -- <> fmap toCssText (maybeToList ts.tagId)

instance CssShow CompositePe where
  toCssText = \case
    AtomicPe x -> toCssText x
    Highlight x -> "::highlight" <> toCssText x
    Part x -> "::part" <> toCssText x
    Picker x -> "::picker" <> toCssText x
    ScrollButton x -> "::scroll-button" <> toCssText x
    Slotted x -> "::slotted" <> toCssText x
    ViewTransitionGroup x -> "::view-transition-group" <> toCssText x
    ViewTransitionImagePair x -> "::view-transition-image-pair" <> toCssText x
    ViewTransitionNew x -> "::view-transition-new" <> toCssText x
    ViewTransitionOld x -> "::view-transition-old" <> toCssText x
