module CssParser.List where

import Control.Arrow (first)
import Data.List.NonEmpty (NonEmpty (..))
import Prelude

unsnocNe :: NonEmpty a -> ([a], a)
unsnocNe (x :| xs) = go x xs
  where
    go y [] = ([], y)
    go y (z : zs) = let ~(ws, w) = go z zs in (y : ws, w)

dropEnd :: Int -> [a] -> [a]
dropEnd i xs
  | i <= 0 = xs
  | otherwise = f xs (drop i xs)
  where
    f (x:xs') (_y:ys) = x : f xs' ys
    f _ _             = []

_initLast :: [a] -> Maybe ([a], a)
_initLast [] = Nothing
_initLast (a : as) = Just (go as a)
  where
    go [] x = ([], x)
    go (y : ys) x = first (x :) (go ys y)
