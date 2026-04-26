module CssParser.At.Keyframe where

import CssParser.Ident ( Ident(..), PropertyName )
import CssParser.Rule.Value
import CssParser.Prelude
import CssParser.Show ( CssShow(..), ShowSpaceBetween(..) )

data KeyframeAdr
  = KeyframePercentAdr Unsigned
  | KeyframeLabel Ident
  deriving (Show, Eq, Ord, Generic)

instance CssShow KeyframeAdr where
  toCssText = \case
    KeyframePercentAdr p -> toCssText $ IntVal p Percent
    KeyframeLabel i -> toCssText i

data PropEntry = PropEntry PropertyName PropVals deriving (Show, Eq, Ord, Generic)
instance ShowSpaceBetween PropEntry PropEntry where
  cssSpace _ _ = ""
instance CssShow PropEntry where
  toCssText (PropEntry pn pv) =
    toCssText pn <> ": " <>  toCssText pv <> ";"

data Keyframe = Keyframe KeyframeAdr [PropEntry] deriving (Show, Eq, Ord, Generic)

instance CssShow Keyframe where
  toCssText (Keyframe kfa ps) =
    toCssText kfa <> " {" <> unwords (toCssText <$> ps) <> "}"
instance ShowSpaceBetween Keyframe Keyframe where
  cssSpace _ _ = " "
newtype KeyframeSetName = KeyframeSetName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

data KeyframeSet = KeyframeSet KeyframeSetName [Keyframe] deriving (Show, Eq, Ord, Generic)

instance CssShow KeyframeSet where
  toCssText (KeyframeSet kfsn frames) =
    "@keyframes " <> toCssText kfsn <> " {" <> toCssText frames <> "}"
