module CssParser.Pseudo where

import CssParser.Rule
    ( Class(NthLastOfType, NthChild, NthOfType, NthLastChild),
      Nth(Nth) )
import Prelude

pattern Even :: Nth
pattern Even = Nth 2 0

pattern Odd :: Nth
pattern Odd = Nth 2 1

pattern One :: Nth
pattern One = Nth 0 1

data NthF
  = NthFChild
  | NthFLastChild
  | NthFLastOfType
  | NthFOfType
  deriving (Show, Eq)

instance Fun NthF (Nth -> Class) where
  call NthFChild = NthChild
  call NthFLastChild = NthLastChild
  call NthFLastOfType = NthLastOfType
  call NthFOfType = NthOfType

data TpmF = TpmIdF | TpmNegF deriving (Show, Eq)

class Fun a f where
  call :: a -> f

instance Num a => Fun TpmF (a -> a) where
  call TpmIdF = id
  call TpmNegF = negate


pattern FirstChildP :: Class
pattern FirstChildP = NthChild One

pattern FirstOfTypeP :: Class
pattern FirstOfTypeP = NthOfType One

pattern LastChildP :: Class
pattern LastChildP = NthLastChild One

pattern LastOfTypeP :: Class
pattern LastOfTypeP = NthLastOfType One
