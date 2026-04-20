module CssParser.Fun where

import CssParser.Prelude ( Eq, Num(negate), Show, id )
import CssParser.Rule
    ( Class(NthLastOfType, NthChild, NthOfType, NthLastChild) )
import CssParser.Rule.Pseudo ( Nth, pattern One )

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
