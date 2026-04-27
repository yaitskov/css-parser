module CssParser.Norm where

import CssParser.Prelude
import GHC.Generics

class GNorm f where
  gnorm :: f a -> f a

instance GNorm V1 where
  gnorm x = case x of {}
instance GNorm U1 where
  gnorm U1 = U1
instance (Norm c) => GNorm (K1 i c) where
  gnorm (K1 a) = K1 $ normalize a
instance (GNorm a, Constructor c) => GNorm (M1 C c a) where
  gnorm (M1 x) = M1 $ gnorm x
instance (GNorm a, Selector s) => GNorm (M1 S s a) where
  gnorm (M1 x) = M1 $ gnorm x
instance (GNorm a) => GNorm (M1 D d a) where
  gnorm (M1 x) = M1 $ gnorm x
instance (GNorm a, GNorm b) => GNorm (a :+: b) where
  gnorm (L1 x) = L1 $ gnorm x
  gnorm (R1 x) = R1 $ gnorm x
instance (GNorm a, GNorm b) => GNorm (a :*: b) where
  gnorm (a :*: b) = gnorm a :*: gnorm b
instance GNorm UChar where
  gnorm = id
instance GNorm UDouble where
  gnorm = id
instance GNorm UFloat where
  gnorm = id
instance GNorm UInt where
  gnorm = id
instance GNorm UWord where
  gnorm = id

class Norm a where
  normalize :: a -> a
  default normalize :: (Generic a, GNorm (Rep a)) => a -> a
  normalize = gNormalizeDefault

gNormalizeDefault :: (Generic a, GNorm (Rep a)) => a -> a
gNormalizeDefault = to . gnorm . from

instance Norm a => Norm [a] where
  normalize = fmap normalize

instance Norm a => Norm (Maybe a) where
  normalize = fmap normalize

instance Norm a => Norm (NonEmpty a) where
  normalize = fmap normalize

instance Norm () where
  normalize = id

instance Norm Text where
  normalize = id

instance Norm LText where
  normalize = id

instance Norm Integer where
  normalize = id

normUntilConst :: (Norm a, Eq a) => a -> a
normUntilConst a =
  let a' = normalize a in
    if a' == a
    then a'
    else normUntilConst a'
