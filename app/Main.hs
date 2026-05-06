module Main where

import Control.Monad ( forM_ )
import CssParser ( parseCss )
import CssParser.Prelude
import System.Environment ( getArgs )

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      ast <- parseCss <$> getContents
      print ast
    cssFiles ->
      forM_ cssFiles $ \cssFile -> do
        ast <- parseCss <$> readFile cssFile
        print ast
