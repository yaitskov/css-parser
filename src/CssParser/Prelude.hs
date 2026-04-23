module CssParser.Prelude
  ( LText
  , Text
  , module X
  ) where

import Data.Char as X
import Data.Either.Combinators as X
import Data.List.NonEmpty as X ( NonEmpty ((:|)), (<|), toList, nonEmpty, appendList, prependList)
import Data.Maybe as X
import Data.String as X (IsString (..))
import Data.Text (Text)
import Data.Text.Lazy as X (concat, intercalate, fromStrict, cons, snoc, unlines, unwords)
import Data.Text.Lazy qualified as L

import GHC.Generics as X (Generic)
import Prelude as X hiding (concat, null, unlines, unwords)

type LText = L.Text
