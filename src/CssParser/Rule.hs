module CssParser.Rule where

import CssParser.Ident
import CssParser.Prelude

data CssRule
  = CssRule (NonEmpty Selector) [CssRuleBodyItem]
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

data AtomicPseudoClass
  = Active  -- ^ The @:active@ pseudo class.
  | Checked  -- ^ The @:checked@ pseudo class.
  | Default  -- ^ The @:default@ pseudo class.
  | Disabled  -- ^ The @:disabled@ pseudo class.
  | Empty         -- ^ The @:empty@ pseudo class.
  | Enabled       -- ^ The @:enabled@ pseudo class.
  | FirstChild
  | FirstOfType
  | Focus         -- ^ The @:focus@ pseudo class.
  | Fullscreen    -- ^ The @:fullscreen@ pseudo class.
  | Hover         -- ^ The @:hover@ pseudo class.
  | Indeterminate -- ^ The @:indeterminate@ pseudo class.
  | InRange     -- ^ The @:in-range@ pseudo class.
  | Invalid     -- ^ The @:invalid@ pseudo class.
  | LastChild
  | LastOfType
  | Link        -- ^ The @:link@ pseudo class.
  | OnlyOfType  -- ^ The @:only-of-type@ pseudo class.
  | OnlyChild  -- ^ The @:only-child@ pseudo class.
  | Optional  -- ^ The @:optional@ pseudo class.
  | OutOfRange  -- ^ The @:out-of-range@ pseudo class.
  | ReadOnly  -- ^ The @:read-only@ pseudo class.
  | ReadWrite  -- ^ The @:rad-write@ pseudo class.
  | Required  -- ^ The @:required@ pseudo class.
  | Root  -- ^ The @:root@ pseudo class.
  | Target  -- ^ The @:target@ pseudo class.
  | Valid  -- ^ The @:valid@ pseudo class.
  | Visited  -- ^ The @:visited@ pseudo class.
  deriving (Eq, Bounded, Enum, Ord, Show, Generic)

newtype Hash = Hash { unHash :: Text } deriving newtype (Eq, Ord, Show, IsString)

data Attr
  = HasAttr AttrName
  | Attr AttrName AttrOp AttrVal
  deriving (Eq, Ord, Show, Generic)

data Nth = Nth { linear :: Int, constant :: Int } deriving (Eq, Ord, Show, Generic)

data CssRuleBodyItem
  = CssLeafRule PropertyName (NonEmpty ())
  | CssNestedRule CssRule
  deriving (Show, Ord, Eq, Generic)

type AttrVal = Text
newtype Language = Language Text deriving newtype (Eq, Ord, Show, IsString)

data AttrOp =
      Exact -- ^ exactly the value of the value, denoted with @=@
    | Include -- ^ whitespace separated list of items, one of these items is the value, denoted with @~=@
    | DashMatch -- ^ hyphen separated list of items, the first item is the value, denoted with @|=@
    | PrefixMatch -- ^ prefix of the value in the attribute, denoted with @^=@
    | SuffixMatch -- ^ suffix of the value in the attribute, denoted with @$=@
    | SubstringMatch -- ^ substring of the value in the attribute, denoted with @*=@
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data PseudoElement
  = After
  | Before
  | FirstLetter
  | FirstLine
  | Marker
  | Placeholder
  | Selection
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)
