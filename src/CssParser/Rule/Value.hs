module CssParser.Rule.Value where

import CssParser.Prelude
import CssParser.Show

newtype Unsigned = Unsigned Integer
  deriving newtype (Eq, Show, Ord, Num, Enum, Real, Read, Integral)
  deriving (Generic)

data Ratio = Ratio Unsigned Unsigned deriving (Eq, Show, Ord, Generic)

instance CssShow Ratio where
  toCssText (Ratio a b) = numToText a <> "/" <> numToText b

readRatio :: String -> Either String Ratio
readRatio s =
  case span (/= '/') s of
    ([], _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    ("/", _) -> Left $ "No divisible in Ratio [" <> s <> "]"
    (_, []) -> Left $ "No divisor in Ratio [" <> s <> "]"
    (_, "/") -> Left $ "No divisor in Ratio [" <> s <> "]"
    (divisibleStr, '/':divisorStr) ->
      Ratio <$> readEither divisibleStr <*> readEither divisorStr
    (_, _) -> Left $ "No slash in ratio [" <> s <> "]"

newtype Url = Url { unUrl :: Text }  deriving newtype (Show, Eq, Ord)
instance CssShow Url where
  toCssText (Url u) = "url(" <> encodeStringLiteral u <> ")"

data Source = UrlSource Url | StrSource Text
  deriving (Show, Eq, Generic)

instance CssShow Source where
  toCssText = \case
    UrlSource u -> toCssText u
    StrSource t -> encodeStringLiteral t
