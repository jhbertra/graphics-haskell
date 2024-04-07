module Physics.Spectrum.Class where

import Data.Functor.Identity (Identity (..))
import Physics.Spectrum.Sampled

class Spectrum s a | s -> a where
  evalSpectrum :: a -> s -> a
  maxValue :: s -> a
  minValue :: s -> a
  scaleSpectrum :: a -> s -> s
  sampleSpectrum :: (HasSpectrumSamples n a) => SampledWavelengths n a -> s -> SampledSpectrum n a
  sampleSpectrum (SampledWavelengths _ λ) s = mapSamples (`evalSpectrum` s) λ
  {-# INLINE sampleSpectrum #-}

instance (Num a) => Spectrum (Identity a) a where
  {-# SPECIALIZE instance Spectrum (Identity Float) Float #-}
  {-# SPECIALIZE instance Spectrum (Identity Double) Double #-}
  evalSpectrum _ = runIdentity
  {-# INLINE evalSpectrum #-}
  maxValue = runIdentity
  {-# INLINE maxValue #-}
  minValue = runIdentity
  {-# INLINE minValue #-}
  scaleSpectrum s (Identity a) = Identity $ s * a
  {-# INLINE scaleSpectrum #-}
  sampleSpectrum _ = fromSample . runIdentity
  {-# INLINE sampleSpectrum #-}

integrateVisible :: (Spectrum s a, RealFrac a) => s -> a
integrateVisible = integrateSpectrum 360 830
{-# INLINE integrateVisible #-}

integrateSpectrum :: (Spectrum s a, RealFrac a) => a -> a -> s -> a
integrateSpectrum a b s = foldr ((+) . flip evalSpectrum s . fromInteger) 0 [floor a .. ceiling b]
{-# INLINE integrateSpectrum #-}
