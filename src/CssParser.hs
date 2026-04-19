module CssParser
  ( parseCss
  , alex
  , module X
  , NonEmpty ((:|))
  ) where

import CssParser.At as X

import CssParser.File as X
import CssParser.Lexer (TokenLoc, alexScanTokens)
import CssParser.Parser as X (cssParser)
import CssParser.Prelude
import CssParser.Rule as X
import CssParser.Show as X

alex :: String -> Either String [TokenLoc]
alex = alexScanTokens

parseCss :: String -> CssFile
parseCss st = al (alex st')
  where
    st' = filter ('\r' /=) st
    al (Left er) = error er
    al (Right val) = cssParser val
