module CssParser.Normal where

import Data.List.NonEmpty
import CssParser.Rule
import Prelude

class Normal a where
  normalize :: a -> a

instance Normal a => Normal [a] where
  normalize = fmap normalize

instance Normal a => Normal (NonEmpty a) where
  normalize = fmap normalize

normalizeNth :: Nth -> Nth
normalizeNth nth@(Nth n c)
  | n <= 0 && c + n <= 0 = Nth 0 (max 0 c)
  | n > 0 && c < 0 = let cn = c `mod` n in if cn /= 0 then Nth n cn else Nth n n
  | n > 0 && c == 0 = Nth n n
  | otherwise = nth
