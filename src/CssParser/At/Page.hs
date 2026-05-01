module CssParser.At.Page where

import CssParser.Ident ( Ident(..) )
import CssParser.Prelude
import CssParser.Rule.Pseudo ( AtomicPseudoClass )
import CssParser.Show ( CssShow(..) )

data PageMargin
  = TopLeftCorner
  | TopLeft
  | TopCenter
  | TopRight
  | TopRightCorner
  | BottomLeftCorner
  | BottomLeft
  | BottomCenter
  | BottomRight
  | BottomRightCorner
  | LeftTop
  | LeftMiddle
  | LeftBottom
  | RightTop
  | RightMiddle
  | RightBottom
  deriving (Eq, Show, Ord, Generic)

instance CssShow PageMargin where
  toCssText = \case
    TopLeftCorner ->      "@top-left-corner"
    TopLeft ->            "@top-left"
    TopCenter ->          "@top-center"
    TopRight ->           "@top-right"
    TopRightCorner ->     "@top-right-corner"
    BottomLeftCorner ->   "@bottom-left-corner"
    BottomLeft ->         "@bottom-left"
    BottomCenter ->       "@bottom-center"
    BottomRight ->        "@bottom-right"
    BottomRightCorner ->  "@bottom-right-corner"
    LeftTop ->            "@left-top"
    LeftMiddle ->         "@left-middle"
    LeftBottom ->         "@left-bottom"
    RightTop ->           "@right-top"
    RightMiddle ->        "@right-middle"
    RightBottom ->        "@right-bottom"

newtype PageName = PageName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

data PageSelector = PageSelector (Maybe PageName) [AtomicPseudoClass]
  deriving (Show, Eq, Ord, Generic)

instance CssShow PageSelector where
  toCssText (PageSelector mpn pps) =
    maybe "" toCssText mpn <> toCssText pps

newtype PageSelectorList = PageSelectorList [PageSelector]
  deriving newtype (Show, Eq, Ord) deriving (Generic)

instance CssShow PageSelectorList where
  toCssText (PageSelectorList pps) =
    unwords ("@page" : (toCssText <$> pps))
