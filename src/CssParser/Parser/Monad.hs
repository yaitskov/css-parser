module CssParser.Parser.Monad where

import CssParser.Prelude
import Data.List qualified as L
import Expression.Reorder

data P a = Ok a | Failed String deriving (Functor)

validationToP :: Show a => a -> Validation (NonEmpty String) a -> P a
validationToP x = \case
  Failure er ->
      Failed $ "Failed to reorder " <> show x <> " due: " <> L.intercalate "\n" (toList er)
  Success a -> Ok a

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

fromEitherM :: Applicative m => (e -> m a) -> Either e a -> m a
fromEitherM ef = \case
  Right v -> pure v
  Left e -> ef e
