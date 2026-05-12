module CssParser.At.Keyframe where

import CssParser.Descriptor (Descriptor)
import CssParser.Ident ( Ident(..) )
import CssParser.Rule.Value ( PropVals )
import CssParser.Rule.TypedNum
import CssParser.Prelude
import CssParser.Show ( CssShow(..), ShowSpaceBetween(..), CslNe )

data KeyframeAdr
  = KeyframePercentAdr RawNum
  | KeyframeStart
  | KeyframeEnd
  deriving (Show, Eq, Ord, Generic)

instance CssShow KeyframeAdr where
  toCssText = \case
    KeyframePercentAdr p -> toCssText $ TypedNum p Percent
    KeyframeStart -> "from"
    KeyframeEnd -> "to"

data PropEntry = PropEntry Descriptor PropVals deriving (Show, Eq, Ord, Generic)
instance ShowSpaceBetween PropEntry PropEntry where
  cssSpace _ _ = ""
instance CssShow PropEntry where
  toCssText (PropEntry pn pv) =
    toCssText pn <> toCssText pv <> ";"

data Keyframe = Keyframe (CslNe KeyframeAdr) [PropEntry] deriving (Show, Eq, Ord, Generic)

instance CssShow Keyframe where
  toCssText (Keyframe kfa ps) =
    toCssText kfa <> " {" <> unwords (toCssText <$> ps) <> "}"
instance ShowSpaceBetween Keyframe Keyframe where
  cssSpace _ _ = " "
newtype KeyframeSetName = KeyframeSetName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

data KeyframeSet
  = KeyframeSet KeyframeSetName [Keyframe]
  deriving (Show, Eq, Ord, Generic)

instance CssShow KeyframeSet where
  toCssText (KeyframeSet kfsn frames) =
    toCssText kfsn <> " {" <> toCssText frames <> "}"
