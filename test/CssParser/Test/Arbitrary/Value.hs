{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Value where

import CssParser.Ident
import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import Data.Text qualified as T

data Anum
  = IntAnum Int
  | PositiveIntAnum Word
  | FloatAnum Int Word
  | EAnum Int Int
  | NoWholeAnum Word
  | EAnum2 Int Word Int
  deriving (Eq, Generic)

instance Show Anum where
  show = \case
    IntAnum x -> show x
    PositiveIntAnum x -> "+" <> show x
    FloatAnum x y -> show x <> "." <> show y
    EAnum x y -> show x <> "e" <> show y
    EAnum2 x y z -> show x <> "." <> show y <> "e" <> show z
    NoWholeAnum x -> "." <> show x

deriving via (GenericArbitrary Anum) instance Arbitrary Anum

instance Arbitrary RawNum where
  arbitrary = mkRawNum . show <$> (arbitrary :: Gen Anum)
  shrink _ = []

instance Arbitrary HexColor where
  arbitrary = HC . pack <$> vectorOf 6 arbitraryHex
  shrink (HC x)
    | T.length x == 6 = [HC $ T.take 3 x]
    | otherwise = []

instance Norm Unsigned where
  normalize = abs

instance Arbitrary Unsigned where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink
instance Arbitrary Url where
  arbitrary = oneof
    [ pure $ Url "https://ooo.com/aoeu/style.css"
    , pure $ UnquotedUrl "https://ooo.com:443/aoeu/style.css?y=3&x=ok#eoeu"
    , pure $ UnquotedUrl "./file.css"
    , pure $ UnquotedUrl "/style.css"
    , pure $ UnquotedUrl "/../style.css"
    ]

deriving via Ident instance Arbitrary LiteralString
deriving via (GenericArbitrary Ratio) instance Arbitrary Ratio
deriving via (GenericArbitrary PropVals) instance Arbitrary PropVals
deriving via (GenericArbitrary PropVal) instance Arbitrary PropVal
deriving via (GenericArbitrary PropValType) instance Arbitrary PropValType
