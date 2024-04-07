module Physics.Spectrum.Add where

import Physics.Spectrum.Class

data AddSpectrum a where
  AddSpectrum :: (Spectrum s a, Spectrum t a) => s -> t -> AddSpectrum a

instance (Num a) => Spectrum (AddSpectrum a) a where
  {-# SPECIALIZE instance Spectrum (AddSpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (AddSpectrum Double) Double #-}
  evalSpectrum λ (AddSpectrum a b) = evalSpectrum λ a + evalSpectrum λ b
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s (AddSpectrum a b) = AddSpectrum (scaleSpectrum s a) (scaleSpectrum s b)
  {-# INLINE scaleSpectrum #-}
  maxValue (AddSpectrum a b) = maxValue a + maxValue b
  {-# INLINE maxValue #-}
  minValue (AddSpectrum a b) = minValue a + minValue b
  {-# INLINE minValue #-}
