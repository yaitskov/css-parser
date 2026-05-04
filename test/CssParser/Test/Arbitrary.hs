{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module CssParser.Test.Arbitrary
  ( module X
  , pack
  , module CssParser.Test.Arbitrary
  )where

import Data.List qualified as L
import Data.HashSet
import Data.Text (pack, tails, inits)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import CssParser.At.MediaQuery
import CssParser.Prelude as X
import CssParser.Show
import Test.QuickCheck as X
import Test.QuickCheck.Gen as X
import Test.QuickCheck.Instances as X ()
import Test.QuickCheck.Arbitrary.Generic as X

enumDomain :: forall a. (Enum a, Bounded a) => [a]
enumDomain = enumFromTo (minBound @a) (maxBound @a)

arbitraryLetter :: Gen Char
arbitraryLetter = elements [ 'a' .. 'z' ]

arbitraryHex :: Gen Char
arbitraryHex = elements $ ['0' .. '9'] <> [ 'a' .. 'f' ] <> [ 'A' .. 'F' ]

arbitraryWord :: Gen Text
arbitraryWord = pack <$> listOf1 arbitraryLetter

keywords :: HashSet Text
keywords = fromList $! initL <> fmap toTxt (enumDomain @MediaType)
  where
    toTxt = toStrict . toCssText
    initL = T.words "not or and only src false true from to url"

arbitraryIdent :: Gen Text
arbitraryIdent = do
  i <- arbitraryText fl nl
  if i `member` keywords
    then pure $ i <> "_"
    else pure i
  where
    fl = ['a' .. 'z'] <> "_"
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
    | otherwise = L.filter (`isMissing` keywords) $ shrinkText t
  where
    isMissing a b = not (a `member` b)

instance Arbitrary a => Arbitrary (Embraced a) where
  arbitrary = Embraced <$> arbitrary
  shrink (Embraced a) =  Embraced <$> shrink a

instance Arbitrary a => Arbitrary (CslNe a) where
  arbitrary = CslNe <$> arbitrary
  shrink (CslNe a) = CslNe <$> shrink a

instance Arbitrary a => Arbitrary (SslNe a) where
  arbitrary = SslNe <$> arbitrary
  shrink (SslNe a) = SslNe <$> shrink a
