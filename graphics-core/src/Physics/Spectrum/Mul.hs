module Physics.Spectrum.Mul where

import Physics.Spectrum.Class

data MulSpectrum a where
  MulSpectrum :: (Spectrum s a, Spectrum t a) => s -> t -> MulSpectrum a

instance (Ord a, Num a) => Spectrum (MulSpectrum a) a where
  {-# SPECIALIZE instance Spectrum (MulSpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (MulSpectrum Double) Double #-}
  evalSpectrum λ (MulSpectrum a b) = evalSpectrum λ a * evalSpectrum λ b
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s (MulSpectrum a b) = MulSpectrum (scaleSpectrum s a) (scaleSpectrum s b)
  {-# INLINE scaleSpectrum #-}
  maxValue (MulSpectrum a b) =
    max maxA $
      max maxB $
        max (maxA * maxB) $
          max (maxA * minB) $
            max (minA * maxB) $
              minA * minB
    where
      maxA = maxValue a
      maxB = maxValue b
      minA = minValue a
      minB = minValue b
  {-# INLINE maxValue #-}
  minValue (MulSpectrum a b) =
    min maxA $
      min maxB $
        min (maxA * maxB) $
          min (maxA * minB) $
            min (minA * maxB) $
              minA * minB
    where
      maxA = maxValue a
      maxB = maxValue b
      minA = minValue a
      minB = minValue b
  {-# INLINE minValue #-}
