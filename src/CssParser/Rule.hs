module CssParser.Rule where

import CssParser.At.FontFace
import CssParser.At.FontFeatureValues
import CssParser.At.Keyframe
import CssParser.At.Layer
import CssParser.At.MediaQuery
import CssParser.At.Page
import CssParser.Ident
import CssParser.Prelude
import CssParser.Rule.Pseudo
import CssParser.Rule.Value

data CssRule
  = CssRule (NonEmpty Selector) [CssRuleBodyItem]
  | MediaRule MediaQueryList [CssRuleBodyItem]
  | LayerBlock (Maybe LayerName) [CssRuleBodyItem]
  | Page PageSelectorList [CssRuleBodyItem]
  | PageMarginBlock PageMargin [CssRuleBodyItem]
  | CounterStyle Ident [CssRuleBodyItem]
  | Property Var [CssRuleBodyItem]
  | Keyframes KeyframeSet
  | ColorProfile PropertyName [PropEntry]
  | FontFaceBlock FontFace
  | FontFeatureValuesBlock FontFeatureValues
  deriving (Show, Ord, Eq, Generic)

data Selector
  = Selector TagSelector [(TagRelation, TagSelector)]
  | PeSelector TagSelector [(TagRelation, TagSelector)] PseudoElement
  | PeSelectorOnly PseudoElement
  deriving (Eq, Ord, Show, Generic)

data TagRelation
  = Descendant
  | Child
  | NextSibling
  | GeneralSibling
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data TagSelector
  = TagSelector
  { tagNs :: Namespace
  , tagName :: TagName
  , tagAttrs :: [Attr]
  , tagId :: Maybe Hash
  , tagClasses :: [Class]
  } deriving (Show, Ord, Eq, Generic)

data Class
  = AtomicClass { unClass :: Ident }
  | AtomicPseudoClass AtomicPseudoClass
  | NotClass (NonEmpty Selector)
  | Lang Language
  | NthChild Nth
  | NthLastChild Nth
  | NthLastOfType Nth
  | NthOfType Nth -- :nth-of-type(<An+B> | even | odd)
  deriving (Eq, Ord, Show, Generic)

newtype Hash = Hash { unHash :: Text } deriving newtype (Eq, Ord, Show, IsString)

data Attr
  = HasAttr AttrName
  | Attr AttrName AttrOp AttrVal
  deriving (Eq, Ord, Show, Generic)

data CssRuleBodyItem
  = CssLeafRule PropertyName PropVals
  | CssNestedRule CssRule
  deriving (Show, Ord, Eq, Generic)

type AttrVal = Text

data AttrOp =
      Exact -- ^ exactly the value of the value, denoted with @=@
    | Include -- ^ whitespace separated list of items, one of these items is the value, denoted with @~=@
    | DashMatch -- ^ hyphen separated list of items, the first item is the value, denoted with @|=@
    | PrefixMatch -- ^ prefix of the value in the attribute, denoted with @^=@
    | SuffixMatch -- ^ suffix of the value in the attribute, denoted with @$=@
    | SubstringMatch -- ^ substring of the value in the attribute, denoted with @*=@
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)
