module Physics.Spectrum.RGB where

import Numeric.FMA
import Physics.Spectrum.Class
import Physics.Spectrum.Interpolated (InterpolatedSpectrum)

data RGBSpectrum a = RGBSpectrum
  { _rgbC0 :: a
  , _rgbC1 :: a
  , _rgbC2 :: a
  , _rgbScale :: a
  , _rgbIlluminant :: Maybe (InterpolatedSpectrum a)
  }
  deriving (Show, Eq, Ord, Functor)

rgbAlbedoSpectrum :: (Num a) => a -> a -> a -> RGBSpectrum a
rgbAlbedoSpectrum c0 c1 c2 = RGBSpectrum c0 c1 c2 1 Nothing

rgbSpectrum :: a -> a -> a -> a -> RGBSpectrum a
rgbSpectrum c0 c1 c2 scale = RGBSpectrum c0 c1 c2 scale Nothing

rgbIlluminantSpectrum :: a -> a -> a -> a -> InterpolatedSpectrum a -> RGBSpectrum a
rgbIlluminantSpectrum c0 c1 c2 scale = RGBSpectrum c0 c1 c2 scale . Just

sigmoid :: (RealFloat a, FMA a) => a -> a
sigmoid a
  | isInfinite a = if a > 0 then 1 else 0
  | otherwise = a / (2 * sqrt (fma a a 1)) + 0.5
{-# INLINE sigmoid #-}

instance (RealFloat a, FMA a) => Spectrum (RGBSpectrum a) a where
  {-# SPECIALIZE instance Spectrum (RGBSpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (RGBSpectrum Double) Double #-}
  evalSpectrum λ RGBSpectrum{..} =
    sigmoid (evalQuadratic _rgbC0 _rgbC1 _rgbC2 λ) * _rgbScale
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s RGBSpectrum{..} =
    RGBSpectrum{_rgbScale = s * _rgbScale, ..}
  {-# INLINE scaleSpectrum #-}
  maxValue RGBSpectrum{..}
    | _rgbScale < 0 = 0
    | otherwise = _rgbScale
  {-# INLINE maxValue #-}
  minValue RGBSpectrum{..}
    | _rgbScale < 0 = _rgbScale
    | otherwise = 0
  {-# INLINE minValue #-}

evalQuadratic :: (FMA a) => a -> a -> a -> a -> a
evalQuadratic a b c x = fma (fma a x b) x c
