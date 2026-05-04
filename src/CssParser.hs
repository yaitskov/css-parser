module CssParser
  ( parseCss
  , parseCssP
  , alex
  , getToken
  , module X
  , NonEmpty ((:|))
  ) where

import CssParser.At as X
import CssParser.At.MediaQuery as X
import CssParser.At.Supports as X hiding (FeatureQuery)
import CssParser.Rule.Value as X
import CssParser.File as X
import CssParser.Ident as X
import CssParser.Lexer (TokenLoc, alexScanTokens, getToken)
import CssParser.Norm as X (Norm (..))
import CssParser.Parser.Monad as X
import CssParser.Parser as X (cssParser)
import CssParser.Prelude
import CssParser.Rule as X
import CssParser.Show as X

alex :: String -> Either String [TokenLoc]
alex = alexScanTokens

parseCssP :: String -> P CssFile
parseCssP st = al (alex st')
  where
    st' = filter ('\r' /=) st
    al (Left er) = error er
    al (Right val) = cssParser val

parseCss :: String -> CssFile
parseCss s =
  case parseCssP s of
    Ok v -> v
    Failed e -> error e
