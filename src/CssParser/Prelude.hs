module CssParser.Prelude
  ( LText
  , Text
  , module X
  ) where

import Data.Char as X
import Data.List.NonEmpty as X ( NonEmpty ((:|)), (<|), toList, nonEmpty, appendList)
import Data.String as X (IsString (..))
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy as X (concat, intercalate, fromStrict, cons, snoc, unlines)

import GHC.Generics as X (Generic)
import Prelude as X hiding (concat, null, unlines)

type LText = L.Text
