module CssParser where

import CssParser.File
import CssParser.Lexer (alexScanTokens)
import CssParser.Parser (cssParser)
import Prelude

parseCss :: String -> CssFile
parseCss st = al (alexScanTokens st')
  where
    st' = filter ('\r' /=) st
    al (Left er) = error er
    al (Right val) = CssFile $ cssParser val
