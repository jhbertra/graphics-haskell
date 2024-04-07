module Physics.Spectrum.Blackbody (
  BlackbodySpectrum,
  blackbodySpectrum,
) where

import Physics.Spectrum.Class

data BlackbodySpectrum a = BlackbodySpectrum
  { _bbT :: a
  , _bbNormalizationFactor :: a
  , _bbScale :: a
  }
  deriving (Show, Eq, Ord, Functor)

blackbodySpectrum :: (Ord a, Floating a) => a -> BlackbodySpectrum a
blackbodySpectrum _bbT = BlackbodySpectrum{..}
  where
    λMax = 2.8977721e-3 / _bbT
    _bbNormalizationFactor = 1 / blackbody _bbT (λMax * 1e9)
    _bbScale = 1

blackbody :: (Ord a, Floating a) => a -> a -> a
blackbody t
  | t <= 0 = const 0
  | otherwise = \λ -> do
      let λ' = λ * 1e-9
      (2 * 5.955206e-17) / ((λ' ** 5) * (exp (1.986443e-25 / (λ' * 1.3806488e-23 * t)) - 1))

instance (RealFloat a) => Spectrum (BlackbodySpectrum a) a where
  {-# SPECIALIZE instance Spectrum (BlackbodySpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (BlackbodySpectrum Double) Double #-}
  evalSpectrum λ BlackbodySpectrum{..}
    | λ < 0 = 0
    | otherwise = blackbody _bbT λ * _bbNormalizationFactor * _bbScale
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s BlackbodySpectrum{..} =
    BlackbodySpectrum{_bbScale = s * _bbScale, ..}
  {-# INLINE scaleSpectrum #-}
  maxValue BlackbodySpectrum{..}
    | _bbScale < 0 = 0
    | otherwise = _bbScale
  {-# INLINE maxValue #-}
  minValue BlackbodySpectrum{..}
    | _bbScale < 0 = _bbScale
    | otherwise = 0
  {-# INLINE minValue #-}
