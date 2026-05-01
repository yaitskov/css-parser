module CssParser.Rule.Pseudo where

import CssParser.Prelude hiding (Left, Right)
import CssParser.Show ( numToText, CssShow(..), ShowSpaceBetween(..) )

newtype Language = Language Text deriving newtype (Eq, Ord, Show, IsString)

data Nth = Nth { linear :: Int, constant :: Int } deriving (Eq, Ord, Show, Generic)

data PseudoElement
  = After
  | Backdrop
  | Before
  | Checkmark
  | Column
  | Cue
  | DetailsContent
  | FileSelectorButton
  | FirstLetter
  | FirstLine
  | GrammarError
  | Marker
  | PickerIcon
  | Placeholder
  | ScrollMarker
  | ScrollMarkerGroup
  | SearchText
  | Selection
  | SpellingError
  | TargetText
  | ViewTransition
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data AtomicPseudoClass
  = Active
  | ActiveViewTransition
  | AnyList
  | Autofill
  | Blank
  | Buffering
  | Checked
  | Current
  | Default
  | Defined
  | Disabled
  | Empty
  | Enabled
  | First
  | FirstChild
  | FirstOfType
  | Focus
  | FocusVisible
  | FocusWithin
  | Fullscreen
  | Future
  | HasSlotted
  | Heading
  | Host
  | Hover
  | Indeterminate
  | InRange
  | InterestSource
  | InterestTarget
  | Invalid
  | LastChild
  | LastOfType
  | Left
  | Link
  | LocalLink
  | Modal
  | Muted
  | OnlyChild
  | OnlyOfType
  | Open
  | Optional
  | OutOfRange
  | Past
  | Paused
  | PictureInPicture
  | PlaceholderShown
  | Playing
  | PopoverOpen
  | ReadOnly
  | ReadWrite
  | Required
  | Right
  | Root
  | Scope
  | Seeking
  | Stalled
  | Target
  | TargetAfter
  | TargetBefore
  | TargetCurrent
  | UserInvalid
  | UserValid
  | Valid
  | Visited
  | VolumeLocked
  | XrOverlay
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

instance ShowSpaceBetween AtomicPseudoClass AtomicPseudoClass where
  cssSpace _ _ = ""

instance CssShow AtomicPseudoClass where
  toCssText = cons ':' . go
    where
      go :: AtomicPseudoClass -> LText
      go = \case
        Active -> "active"
        ActiveViewTransition -> "active-view-transition"
        AnyList -> "any-list"
        Autofill -> "autofill"
        Blank -> "blank"
        Buffering -> "buffering"
        Checked -> "checked"
        Current -> "current"
        Default -> "default"
        Defined -> "defined"
        Disabled -> "disabled"
        Empty -> "empty"
        Enabled -> "enabled"
        First -> "first"
        FirstChild -> "first-child"
        FirstOfType -> "first-of-type"
        Focus -> "focus"
        FocusVisible -> "focus-visible"
        FocusWithin -> "focus-within"
        Fullscreen -> "fullscreen"
        Future -> "future"
        HasSlotted -> "has-slotted"
        Heading -> "heading"
        Host -> "host"
        Hover -> "hover"
        Indeterminate -> "indeterminate"
        InRange -> "in-range"
        InterestSource -> "interest-source"
        InterestTarget -> "interest-target"
        Invalid -> "invalid"
        LastChild -> "last-child"
        LastOfType -> "last-of-type"
        Left -> "left"
        Link -> "link"
        LocalLink -> "local-link"
        Modal -> "modal"
        Muted -> "muted"
        OnlyChild -> "only-child"
        OnlyOfType -> "only-of-type"
        Open -> "open"
        Optional -> "optional"
        OutOfRange -> "out-of-range"
        Past -> "past"
        Paused -> "paused"
        PictureInPicture -> "picture-in-picture"
        PlaceholderShown -> "placeholder-shown"
        Playing -> "playing"
        PopoverOpen -> "popover-open"
        ReadOnly -> "read-only"
        ReadWrite -> "read-write"
        Required -> "required"
        Right -> "right"
        Root -> "root"
        Scope -> "scope"
        Seeking -> "seeking"
        Stalled -> "stalled"
        Target -> "target"
        TargetAfter -> "target-after"
        TargetBefore -> "target-before"
        TargetCurrent -> "target-current"
        UserInvalid -> "user-invalid"
        UserValid -> "user-valid"
        Valid -> "valid"
        Visited -> "visited"
        VolumeLocked -> "volume-locked"
        XrOverlay -> "xr-overlay"

instance CssShow PseudoElement where
  toCssText = ("::" <>) . go
    where
      go = \case
        After -> "after"
        Backdrop -> "backdrop"
        Before -> "before"
        Checkmark -> "checkmark"
        Column -> "column"
        Cue -> "cue"
        DetailsContent -> "details-content"
        FileSelectorButton -> "file-selector-button"
        FirstLetter -> "first-letter"
        FirstLine -> "first-line"
        GrammarError -> "grammar-error"
        Marker -> "marker"
        PickerIcon -> "picker-icon"
        Placeholder -> "placeholder"
        ScrollMarker -> "scroll-marker"
        ScrollMarkerGroup -> "scroll-marker-group"
        SearchText -> "search-text"
        Selection -> "selection"
        SpellingError -> "spelling-error"
        TargetText -> "target-text"
        ViewTransition -> "view-transition"
