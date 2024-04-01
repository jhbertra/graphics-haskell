module Geometry.Barycentric where

import GHC.Generics (Generic)
import Linear
import Numeric.FMA

data Barycentric a = Barycentric
  { _b0 :: a
  , _b1 :: a
  , _b2 :: a
  }
  deriving (Show, Read, Eq, Ord, Functor, Generic)

instance Applicative Barycentric where
  pure a = Barycentric a a a
  {-# INLINE pure #-}
  Barycentric f0 f1 f2 <*> Barycentric a0 a1 a2 = Barycentric (f0 a0) (f1 a1) (f2 a2)
  {-# INLINE (<*>) #-}

instance Additive Barycentric where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

berp :: (FMA a) => a -> a -> a -> Barycentric a -> a
berp a b c Barycentric{..} = fma a _b0 $ sumOfProducts b _b1 c _b2

berpF :: (Num a, Additive f) => f a -> f a -> f a -> Barycentric a -> f a
berpF a b c Barycentric{..} = a ^* _b0 ^+^ b ^* _b1 ^+^ c ^* _b2
