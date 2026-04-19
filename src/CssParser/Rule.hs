module CssParser.Rule where

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.String (IsString)
import Data.Text qualified as TS
import GHC.Generics (Generic)
import Prelude

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

newtype Ident = Ident TS.Text deriving newtype (Eq, Ord, Show, IsString)

data TagName
  = TagName Ident
  | AmpersandTag
  | AsteriskTag
  | NoTag
  deriving (Eq, Ord, Show, Generic)

data Namespace
  = NoBar
  | NoNs
  | AsteriskNs
  | Namespace Ident
  deriving (Eq, Ord, Show, Generic)

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

newtype Hash = Hash { unHash :: TS.Text } deriving newtype (Eq, Ord, Show, IsString)

data Attr
  = HasAttr AttrName
  | Attr AttrName AttrOp AttrVal
  deriving (Eq, Ord, Show, Generic)

data AttrName
  = AttrName
  { attrNs :: Namespace
  , attrName :: Ident
  } deriving (Eq, Ord, Show, Generic)

data Nth = Nth { linear :: Int, constant :: Int } deriving (Eq, Ord, Show, Generic)

data CssRuleBodyItem
  = CssLeafRule PropertyName (NonEmpty ())
  | CssNestedRule CssRule
  deriving (Show, Ord, Eq, Generic)

newtype PropertyName = PropertyName Ident deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)

type AttrVal = TS.Text
newtype Language = Language TS.Text deriving newtype (Eq, Ord, Show, IsString)

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

updateTopTagSelector :: (TagSelector -> TagSelector) -> CssRule -> CssRule
updateTopTagSelector tsF (CssRule (fts :| ots) body) =
  CssRule (updateFirstTagSelector tsF fts :| ots) body

setTsNs :: Ident -> TagSelector -> TagSelector
setTsNs ns ts = ts { tagNs = Namespace ns }

prependSelectorToRule :: Ident -> CssRule -> CssRule
prependSelectorToRule iden (CssRule ss body) =
  CssRule (selectorByTag iden <| ss) body

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
prependIdentToRule tn tr (CssRule (fts :| ots) body) =
  CssRule (prependIdent tn tr fts :| ots) body

prependAttr :: Attr -> CssRule -> CssRule
prependAttr atr = updateTopTagSelector go
  where
    go ts = ts { tagAttrs = atr : ts.tagAttrs }

setTopTagName :: Ident -> CssRule -> CssRule
setTopTagName tn = updateTopTagSelector go
  where
    go ts = ts { tagName = TagName tn }
