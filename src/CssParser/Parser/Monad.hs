module CssParser.Parser.Monad where

import CssParser.Prelude

data P a = Ok a | Failed String deriving (Functor)

instance Applicative P where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Failed f <*> _ = Failed f
  _ <*> Failed b = Failed b

instance Monad P where
  Failed v >>= _ = Failed v
  Ok v >>= f = f v

instance MonadFail P where
  fail = Failed

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
