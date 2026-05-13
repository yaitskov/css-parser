module CssParser.At.CustomMedia where

import CssParser.At.MediaQuery ( MediaQueryList )
import CssParser.Prelude ( Eq, Ord, Show, Generic, Bool )
import CssParser.Show ( CssShow(..) )


data CustomMediaQuery
  = CustomMediaFlag Bool
  | CustomMediaQuery MediaQueryList
  deriving (Eq, Show, Ord, Generic)

instance CssShow CustomMediaQuery where
  toCssText = \case
    CustomMediaFlag f ->  toCssText f
    CustomMediaQuery mq -> toCssText mq
