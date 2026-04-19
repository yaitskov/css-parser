module CssParser.Import where

import CssParser.Rule
import CssParser.Parser
import Data.Text (Text)
import Prelude

newtype Url = Url { unUrl :: Text } newtype deriving (Show, Eq, Ord)

data ImportSource
  = ImportSourceUrl Url
  | ImportSourceStr Text

data ImportLayer
  =
data Import
  = ImportUrl ImportSource
  |
