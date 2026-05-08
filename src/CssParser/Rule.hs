module CssParser.Rule where

import CssParser.At.Container ( ContainerQueryMap )
import CssParser.At.FontFace ( FontFace, CommaSeparatedList )
import CssParser.At.FontFeatureValues ( FontFeatureValues )
import CssParser.At.FontPaletteValues ( FontPaletteValues )
import CssParser.At.Function ( Function )
import CssParser.At.Import ( Import )
import CssParser.At.Keyframe ( KeyframeSet, PropEntry )
import CssParser.At.MediaQuery ( MediaQueryList )
import CssParser.At.Page ( PageMargin, PageSelectorList )
import CssParser.At.Supports qualified as S
import CssParser.Ident
import CssParser.MonoPair ( MonoPair )
import CssParser.Prelude
import CssParser.Rule.Pseudo
    ( AtomicPseudoClass, PseudoElement, Nth, Language )
import CssParser.Rule.Value
    ( PropVals, PropValsList, Source, Unsigned )
import CssParser.Show ( CslNe, Embraced, SslNe )

type SelectorList = NonEmpty Selector
type FeatureQuery = S.FeatureQuery SelectorList

data CssRule
  = CssRule SelectorList [CssRuleBodyItem]
  | AtRule BrowserPrefix AtRule
  deriving (Show, Ord, Eq, Generic)

data AtRule
  = MediaRule MediaQueryList [CssRuleBodyItem]
  | LayerBlock (Maybe LayerName) [CssRuleBodyItem]
  | LayerStmt (NonEmpty LayerName)
  | ImportStmt (Import SelectorList)
  | Namespace (Maybe Ident) Source
  | CharsetStmt Charset
  | Page PageSelectorList [CssRuleBodyItem]
  | PageMarginBlock PageMargin [CssRuleBodyItem]
  | CounterStyle Ident [CssRuleBodyItem]
  | Property Var [CssRuleBodyItem]
  | Keyframes KeyframeSet
  | ColorProfile PropertyName [PropEntry]
  | FontFaceBlock FontFace
  | FontFeatureValuesBlock FontFeatureValues
  | FontPaletteValuesBlock FontPaletteValues
  | Container ContainerQueryMap [CssRuleBodyItem]
  | PositionTry Var [PropEntry]
  | StartingStyle [CssRuleBodyItem]
  | ViewTransition [CssRuleBodyItem]
  | ScopeBlock (MonoPair SelectorList) [CssRuleBodyItem]
  | Supports FeatureQuery [CssRuleBodyItem]
  | FunctionBlock CssFunction
  | UnknownGramma Ident (Maybe CommaSeparatedList) [CssRuleBodyItem]
  deriving (Show, Ord, Eq, Generic)

type CssFunction = Function CssRule

data Selector
  = Selector (Maybe TagRelation) TagSelector [(TagRelation, TagSelector)]
  | PeSelector (Maybe TagRelation) TagSelector [(TagRelation, TagSelector)] PseudeTagSelector
  | PeSelectorOnly PseudeTagSelector
  deriving (Eq, Ord, Show, Generic)

data PseudeTagSelector
  = PseudeTagSelector
  { ptagName :: CompositePe
  , ptagSubs :: [TagSubSelector]
  } deriving (Eq, Ord, Show, Generic)

data CompositePe
  = AtomicPe PseudoElement
  | Highlight (Embraced Ident)
  | Part (Embraced (SslNe Ident))
  | Picker (Embraced Ident)
  | ScrollButton (Embraced TagName)
  | Slotted (Embraced SelectorList)
  | ViewTransitionGroup (Embraced SelectorList)
  | ViewTransitionImagePair (Embraced SelectorList)
  | ViewTransitionNew (Embraced SelectorList)
  | ViewTransitionOld (Embraced SelectorList)
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
  , tagSubSelectors :: [ TagSubSelector ]
  } deriving (Show, Ord, Eq, Generic)

data TagSubSelector
  = AtomicClass { unClass :: Ident }
  | AtomicPseudoClass AtomicPseudoClass
  | NotClass SelectorList
  | Lang Language
  | Where SelectorList
  | Has SelectorList
  | Is SelectorList
  | NthChild Nth
  | NthLastChild Nth
  | NthLastOfType Nth
  | NthOfType Nth -- :nth-of-type(<An+B> | even | odd)
  | ActiveViewTransitionType (Embraced (CslNe Ident))
  | Dir (Embraced Ident)
  | Heading (Embraced (CslNe Unsigned))
  | Host (Embraced SelectorList)
  | State (Embraced Ident)

  | HasAttr AttrName
  | Attr AttrName AttrOp AttrVal
  | Hash Ident

  deriving (Eq, Ord, Show, Generic)

data CssRuleBodyItem
  = CssLeafRule PropertyName PropVals
  | CssEnumLeaf PropertyName PropValsList
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
