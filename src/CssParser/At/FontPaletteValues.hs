module CssParser.At.FontPaletteValues where

import CssParser.At.Keyframe ( PropEntry )
import CssParser.Ident ( Var )
import CssParser.Prelude
import CssParser.Show ( CssShow(..) )


data FontPaletteValues
  = FontPaletteValues Var [PropEntry]
  deriving (Show, Eq, Ord, Generic)

instance CssShow FontPaletteValues where
  toCssText (FontPaletteValues v ps) =
    "@font-palette-values " <> toCssText v <> " {" <> toCssText ps <> "}"
