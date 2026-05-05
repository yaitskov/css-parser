module Main where

import CssParser
import CssParser.Prelude

main :: IO ()
main = do
  ast <- parseCss <$> getContents
  print ast
