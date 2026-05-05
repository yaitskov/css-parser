module CssParser.Fun where

import CssParser.Prelude ( Eq, Num(negate), Show, id )
import CssParser.Rule
    ( TagSubSelector(NthLastOfType, NthChild, NthOfType,
                     NthLastChild) )
import CssParser.Rule.Pseudo ( Nth, pattern One )

data NthF
  = NthFChild
  | NthFLastChild
  | NthFLastOfType
  | NthFOfType
  deriving (Show, Eq)

instance Fun NthF (Nth -> TagSubSelector) where
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

pattern FirstChildP :: TagSubSelector
pattern FirstChildP = NthChild One

pattern FirstOfTypeP :: TagSubSelector
pattern FirstOfTypeP = NthOfType One

pattern LastChildP :: TagSubSelector
pattern LastChildP = NthLastChild One

pattern LastOfTypeP :: TagSubSelector
pattern LastOfTypeP = NthLastOfType One
