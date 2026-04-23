module CssParser.Parser.Monad where

import CssParser.Prelude ( String )

data P a = Ok a | Failed String

thenP :: P a -> (a -> P b) -> P b
m `thenP` k =
   case m of
       Ok a     -> k a
       Failed e -> Failed e

returnP :: a -> P a
returnP = Ok

failP :: String -> P a
failP = Failed

catchP :: P a -> (String -> P a) -> P a
catchP m k =
   case m of
      Ok a     -> Ok a
      Failed e -> k e
