{-# LANGUAGE QuantifiedConstraints #-}

module Numeric.FMA where

import Data.Coerce (coerce)
import Foreign.C
import Geometry.Normal
import Linear
import Linear.Affine

class (Num a) => FMA a where
  fma :: a -> a -> a -> a

foreign import ccall "math.h fmaf"
  c_fmaf :: CFloat -> CFloat -> CFloat -> CFloat

foreign import ccall "math.h fma"
  c_fma :: CDouble -> CDouble -> CDouble -> CDouble

instance (FMA a) => FMA (V2 a) where
  {-# SPECIALIZE instance FMA (V2 Float) #-}
  {-# SPECIALIZE instance FMA (V2 Double) #-}
  fma a b c = fma <$> a <*> b <*> c
  {-# INLINE fma #-}

instance (FMA a) => FMA (V3 a) where
  {-# SPECIALIZE instance FMA (V3 Float) #-}
  {-# SPECIALIZE instance FMA (V3 Double) #-}
  fma a b c = fma <$> a <*> b <*> c
  {-# INLINE fma #-}

instance (FMA a, forall x. (FMA x) => FMA (f x)) => FMA (Point f a) where
  {-# SPECIALIZE instance FMA (Point V2 Float) #-}
  {-# SPECIALIZE instance FMA (Point V2 Double) #-}
  {-# SPECIALIZE instance FMA (Point V3 Float) #-}
  {-# SPECIALIZE instance FMA (Point V3 Double) #-}
  fma = coerce $ fma @(f a)
  {-# INLINE fma #-}

instance (FMA a, forall x. (FMA x) => FMA (f x)) => FMA (Normal f a) where
  {-# SPECIALIZE instance FMA (Normal V2 Float) #-}
  {-# SPECIALIZE instance FMA (Normal V2 Double) #-}
  {-# SPECIALIZE instance FMA (Normal V3 Float) #-}
  {-# SPECIALIZE instance FMA (Normal V3 Double) #-}
  fma = coerce $ fma @(f a)
  {-# INLINE fma #-}

instance FMA Float where
  {-# SPECIALIZE instance FMA Float #-}
  fma = coerce c_fmaf
  {-# INLINE fma #-}

instance FMA Double where
  {-# SPECIALIZE instance FMA Double #-}
  fma = coerce c_fma
  {-# INLINE fma #-}

fms :: (FMA a) => a -> a -> a -> a
fms a b c = fma a b (-c)
{-# INLINE fms #-}

sumOfProducts :: (FMA a) => a -> a -> a -> a -> a
sumOfProducts a b c d = sop + err
  where
    cd = c * d
    sop = fma a b cd
    err = fma c d (-cd)
{-# INLINE sumOfProducts #-}

differenceOfProducts :: (FMA a) => a -> a -> a -> a -> a
differenceOfProducts a b c d = sop + err
  where
    cd = c * d
    sop = fma a b (-cd)
    err = fma (-c) d cd
{-# INLINE differenceOfProducts #-}
