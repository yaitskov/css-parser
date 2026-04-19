module CssParser.Show
  ( module X
  , CssShow (..)
  ) where

import CssParser.TextMarshal as X
import CssParser.Rule
import CssParser.Pseudo ( pattern Odd, pattern Even )
import CssParser.Utils ( encodeIdentifier )
import Data.Text.Lazy (pack)
import CssParser.Prelude

class CssShow a where
  toCssText :: a -> LText

instance CssShow a => CssShow [a] where
  toCssText = concat . fmap toCssText
{- HLINT ignore "Use concatMap" -}

instance CssShow a => CssShow (NonEmpty a) where
  toCssText = concat . toList . fmap toCssText

instance CssShow CssRule where
  toCssText (CssRule sels body) =
    intercalate ", " (toList $ fmap toCssText sels) <> "{" <> toCssText body <> "}"

instance CssShow CssRuleBodyItem where
  toCssText = \case
    CssNestedRule cr -> toCssText cr
    CssLeafRule pn pv -> toCssText pn <> ": " <> toCssText pv <> "; "

instance CssShow () where
  toCssText () = " 0 ";

instance CssShow PropertyName where
  toCssText (PropertyName (Ident pn)) = fromStrict pn

instance CssShow TagRelation where
  toCssText = \case
    Descendant -> " "
    Child -> " > "
    NextSibling -> " + "
    GeneralSibling -> " ~ "

instance CssShow Nth where
  toCssText = \case
    Even -> "even"
    Odd -> "odd"
    (Nth n 0) -> snoc (pack $ show n) 'n'
    (Nth 0 b) -> pack (show b)
    (Nth n b)
      | b <= 0 -> pack (show n ++ 'n' : show b)
      | otherwise -> pack (show n ++ 'n' : '+' : show b)

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

instance CssShow Namespace where
  toCssText = \case
    NoBar -> ""
    NoNs  -> "|"
    AsteriskNs -> "*|"
    Namespace (Ident t) -> encodeIdentifier t <> "|"

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
    ]

instance CssShow TagName where
  toCssText = \case
    NoTag -> ""
    AsteriskTag -> "*"
    AmpersandTag -> "&"
    TagName (Ident lt) -> fromStrict lt

instance CssShow AtomicPseudoClass where
  toCssText = cons ':' . go
    where
      go :: AtomicPseudoClass -> LText
      go = \case
        Active -> "active"
        Checked -> "checked"
        Default -> "default"
        Disabled -> "disabled"
        Empty -> "empty"
        Enabled -> "enabled"
        Focus -> "focus"
        Fullscreen -> "fullscreen"
        Hover -> "hover"
        Indeterminate -> "indeterminate"
        InRange -> "in-range"
        Invalid -> "invalid"
        Link -> "link"
        FirstChild -> "first-child"
        LastChild -> "last-child"
        LastOfType -> "last-of-type"
        FirstOfType -> "first-of-type"
        OnlyOfType -> "only-of-type"
        OnlyChild -> "only-child"
        Optional -> "optional"
        OutOfRange -> "out-of-range"
        ReadOnly -> "read-only"
        ReadWrite -> "read-write"
        Required -> "required"
        Root -> "root"
        Target -> "target"
        Valid -> "valid"
        Visited -> "visited"

instance CssShow PseudoElement where
  toCssText = ("::" <>) . go
    where
      go = \case
        After -> "after"
        Before -> "before"
        FirstLetter -> "first-letter"
        FirstLine -> "first-line"
        Marker -> "marker"
        Placeholder -> "placeholder"
        Selection -> "selection"
