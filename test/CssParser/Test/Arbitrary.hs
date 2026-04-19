{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary
  ( module X
  , pack
  , module CssParser.Test.Arbitrary
  )where

import Data.Text (pack, tails, inits)
import Data.Text qualified as T
import CssParser.Prelude as X
import Test.QuickCheck as X
import Test.QuickCheck.Gen as X
import Test.QuickCheck.Instances as X ()
import Test.QuickCheck.Arbitrary.Generic as X

arbitraryLetter :: Gen Char
arbitraryLetter = elements $ [ 'a' .. 'z' ] <> [ 'A' .. 'Z' ]

arbitraryWord :: Gen Text
arbitraryWord = pack <$> listOf1 arbitraryLetter

arbitraryIdent :: Gen Text
arbitraryIdent = arbitraryText fl nl
  where
    fl = ['a' .. 'z'] <> ['A' .. 'Z'] <> "_"
    nl = fl <> ['-', '0' .. '9']

arbitraryText :: String -> String -> Gen Text
arbitraryText fl nl = do
  fc <- elements fl
  pack . (fc :) <$> listOf (elements nl)

shrinkText :: Text -> [Text]
shrinkText = liftA2 (zipWith (<>)) inits (tails . T.drop 1)

shrinkIdent :: Text -> [Text]
shrinkIdent t
    | T.length t < 2 = []
    | otherwise = shrinkText t
