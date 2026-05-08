module CssParser.File where

import CssParser.Prelude
import CssParser.Rule ( CssRule )
import CssParser.Rule.Show ()
import CssParser.Show ( CssShow(..) )

newtype CssFile = CssFile { rules :: [ CssRule ] } deriving (Show, Eq, Generic)

instance CssShow CssFile where
  toCssText cf = unlines (toCssText <$> cf.rules)
