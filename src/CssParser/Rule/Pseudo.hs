module CssParser.Rule.Pseudo where

import CssParser.Prelude
import CssParser.Show ( CssShow(..), numToText )

newtype Language = Language Text deriving newtype (Eq, Ord, Show, IsString)

data Nth = Nth { linear :: Int, constant :: Int } deriving (Eq, Ord, Show, Generic)

data PseudoElement
  = After
  | Before
  | FirstLetter
  | FirstLine
  | Marker
  | Placeholder
  | Selection
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

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

pattern Even :: Nth
pattern Even = Nth 2 0

pattern Odd :: Nth
pattern Odd = Nth 2 1

pattern One :: Nth
pattern One = Nth 0 1

instance CssShow Nth where
  toCssText = \case
    Even -> "even"
    Odd -> "odd"
    (Nth n 0) -> snoc (numToText n) 'n'
    (Nth 0 b) -> numToText b
    (Nth n b)
      | b <= 0 -> numToText n <> "n" <> numToText b
      | otherwise -> numToText n <> "n+" <> numToText b

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
