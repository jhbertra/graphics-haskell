{-# LANGUAGE StrictData #-}

module Physics.Spectrum.Sampled where

import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.Ord (clamp)
import GHC.Exception (ArithException (Overflow), throw)
import GHC.TypeLits (KnownNat, natVal)
import Geometry.Bounds ()
import Numeric.FMA

class (Floating (SampledSpectrum n a)) => HasSpectrumSamples n a where
  data SampledSpectrum n a :: Type
  headSample :: SampledSpectrum n a -> a
  (!) :: SampledSpectrum n a -> Int -> a
  fromSample :: a -> SampledSpectrum n a
  mapSamples :: (a -> a) -> SampledSpectrum n a -> SampledSpectrum n a
  lift2Samples :: (a -> a -> a) -> SampledSpectrum n a -> SampledSpectrum n a -> SampledSpectrum n a
  foldlSamples :: (b -> a -> b) -> b -> SampledSpectrum n a -> b
  foldrSamples :: (a -> b -> b) -> b -> SampledSpectrum n a -> b
  iterateSamples :: (a -> a) -> a -> SampledSpectrum n a
  unfoldrSamples :: (b -> (a, b)) -> b -> SampledSpectrum n a
  terminateSecondarySamples :: SampledSpectrum n a -> SampledSpectrum n a
  secondaryTerminatedSamples :: SampledSpectrum n a -> Bool
  maxSample :: SampledSpectrum n a -> a
  default maxSample :: (Ord a, Bounded a) => SampledSpectrum n a -> a
  maxSample = foldlSamples max minBound
  {-# INLINE maxSample #-}
  minSample :: SampledSpectrum n a -> a
  default minSample :: (Ord a, Bounded a) => SampledSpectrum n a -> a
  minSample = foldlSamples min maxBound
  {-# INLINE minSample #-}
  meanSample :: SampledSpectrum n a -> a
  clampSamples :: a -> a -> SampledSpectrum n a -> SampledSpectrum n a
  default clampSamples :: (Ord a) => a -> a -> SampledSpectrum n a -> SampledSpectrum n a
  clampSamples lo hi = mapSamples $ clamp (lo, hi)
  {-# INLINE clampSamples #-}
  safeDiv :: SampledSpectrum n a -> SampledSpectrum n a -> SampledSpectrum n a
  default safeDiv :: (Eq a, Fractional a) => SampledSpectrum n a -> SampledSpectrum n a -> SampledSpectrum n a
  safeDiv = lift2Samples \a b -> if b == 0 then 0 else a / b
  {-# INLINE safeDiv #-}

instance HasSpectrumSamples 4 Float where
  {-# SPECIALIZE instance HasSpectrumSamples 4 Float #-}
  data SampledSpectrum 4 Float
    = SpectrumSamples4F {-# UNPACK #-} Float {-# UNPACK #-} Float {-# UNPACK #-} Float {-# UNPACK #-} Float
  headSample (SpectrumSamples4F a _ _ _) = a
  {-# INLINE headSample #-}
  (SpectrumSamples4F a _ _ _) ! 0 = a
  (SpectrumSamples4F _ a _ _) ! 1 = a
  (SpectrumSamples4F _ _ a _) ! 2 = a
  (SpectrumSamples4F _ _ _ a) ! 3 = a
  _ ! _ = throw Overflow
  {-# INLINE (!) #-}
  fromSample a = SpectrumSamples4F a a a a
  {-# INLINE fromSample #-}
  mapSamples f (SpectrumSamples4F a b c d) = SpectrumSamples4F (f a) (f b) (f c) (f d)
  {-# INLINE mapSamples #-}
  lift2Samples f (SpectrumSamples4F a0 a1 a2 a3) (SpectrumSamples4F b0 b1 b2 b3) =
    SpectrumSamples4F (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3)
  {-# INLINE lift2Samples #-}
  foldlSamples f seed (SpectrumSamples4F a b c d) = f (f (f (f seed a) b) c) d
  {-# INLINE foldlSamples #-}
  foldrSamples f seed (SpectrumSamples4F a b c d) = f a $ f b $ f c $ f d seed
  {-# INLINE foldrSamples #-}
  meanSample (SpectrumSamples4F a b c d) = (a + b + c + d) / 4
  {-# INLINE meanSample #-}
  terminateSecondarySamples (SpectrumSamples4F a _ _ _) = SpectrumSamples4F a 0 0 0
  {-# INLINE terminateSecondarySamples #-}
  secondaryTerminatedSamples (SpectrumSamples4F _ b c d) = b == 0 && c == 0 && d == d
  {-# INLINE secondaryTerminatedSamples #-}
  iterateSamples f a0 =
    let a1 = f a0
        a2 = f a1
        a3 = f a2
     in SpectrumSamples4F a0 a1 a2 a3
  {-# INLINE iterateSamples #-}
  unfoldrSamples f b0 =
    let (a0, b1) = f b0
        (a1, b2) = f b1
        (a2, b3) = f b2
        (a3, _) = f b3
     in SpectrumSamples4F a0 a1 a2 a3
  {-# INLINE unfoldrSamples #-}

deriving instance Show (SampledSpectrum 4 Float)
deriving instance Read (SampledSpectrum 4 Float)
deriving instance Eq (SampledSpectrum 4 Float)
deriving instance Ord (SampledSpectrum 4 Float)

instance Num (SampledSpectrum 4 Float) where
  {-# SPECIALIZE instance Num (SampledSpectrum 4 Float) #-}
  (+) = lift2Samples (+)
  {-# INLINE (+) #-}
  (-) = lift2Samples (+)
  {-# INLINE (-) #-}
  (*) = lift2Samples (+)
  {-# INLINE (*) #-}
  abs = mapSamples abs
  {-# INLINE abs #-}
  negate = mapSamples negate
  {-# INLINE negate #-}
  signum = mapSamples signum
  {-# INLINE signum #-}
  fromInteger = fromSample . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional (SampledSpectrum 4 Float) where
  {-# SPECIALIZE instance Fractional (SampledSpectrum 4 Float) #-}
  (/) = lift2Samples (/)
  {-# INLINE (/) #-}
  recip = mapSamples recip
  {-# INLINE recip #-}
  fromRational = fromSample . fromRational
  {-# INLINE fromRational #-}

instance Floating (SampledSpectrum 4 Float) where
  {-# SPECIALIZE instance Floating (SampledSpectrum 4 Float) #-}
  pi = fromSample pi
  {-# INLINE pi #-}
  (**) = lift2Samples (**)
  {-# INLINE (**) #-}
  exp = mapSamples exp
  {-# INLINE exp #-}
  log = mapSamples log
  {-# INLINE log #-}
  logBase = lift2Samples logBase
  {-# INLINE logBase #-}
  sqrt = mapSamples sqrt
  {-# INLINE sqrt #-}
  sin = mapSamples sin
  {-# INLINE sin #-}
  cos = mapSamples cos
  {-# INLINE cos #-}
  tan = mapSamples tan
  {-# INLINE tan #-}
  asin = mapSamples asin
  {-# INLINE asin #-}
  acos = mapSamples acos
  {-# INLINE acos #-}
  atan = mapSamples atan
  {-# INLINE atan #-}
  sinh = mapSamples sinh
  {-# INLINE sinh #-}
  cosh = mapSamples cosh
  {-# INLINE cosh #-}
  tanh = mapSamples tanh
  {-# INLINE tanh #-}
  asinh = mapSamples asinh
  {-# INLINE asinh #-}
  acosh = mapSamples acosh
  {-# INLINE acosh #-}
  atanh = mapSamples atanh
  {-# INLINE atanh #-}

data SampledWavelengths n a = SampledWavelengths (SampledSpectrum n a) (SampledSpectrum n a)

deriving instance (Show a, Show (SampledSpectrum n a)) => Show (SampledWavelengths n a)
deriving instance (Read a, Read (SampledSpectrum n a)) => Read (SampledWavelengths n a)
deriving instance (Eq a, Eq (SampledSpectrum n a)) => Eq (SampledWavelengths n a)
deriving instance (Ord a, Ord (SampledSpectrum n a)) => Ord (SampledWavelengths n a)

sampledWavelengthsPdf :: SampledWavelengths n a -> SampledSpectrum n a
sampledWavelengthsPdf (SampledWavelengths pdf _) = pdf
{-# INLINE sampledWavelengthsPdf #-}

terminateSecondary
  :: forall n a
   . (KnownNat n, HasSpectrumSamples n a)
  => SampledWavelengths n a
  -> SampledWavelengths n a
terminateSecondary sw@(SampledWavelengths pdf λ)
  | secondaryTerminatedSamples pdf = sw
  | otherwise = SampledWavelengths (terminateSecondarySamples pdf / fromInteger n) λ
  where
    n = natVal $ Proxy @n
{-# INLINE terminateSecondary #-}

secondaryTerminated :: (HasSpectrumSamples n a) => SampledWavelengths n a -> Bool
secondaryTerminated (SampledWavelengths pdf _) = secondaryTerminatedSamples pdf
{-# INLINE secondaryTerminated #-}

sampleUniformWavelengths
  :: forall n a
   . ( KnownNat n
     , Ord a
     , Fractional a
     , HasSpectrumSamples n a
     )
  => a
  -> a
  -> a
  -> SampledWavelengths n a
sampleUniformWavelengths λMin λMax ξ =
  SampledWavelengths
    (fromSample $ recip $ λMax - λMin)
    (iterateSamples λNext $ (1 - ξ) * λMin + ξ * λMax)
  where
    λNext λ
      | λ' > λMax = λMin + λ' - λMax
      | otherwise = λ'
      where
        λ' = λ + δ
    δ = (λMax - λMin) / fromInteger n
    n = natVal $ Proxy @n

sampleVisibleWavelengths
  :: forall n a
   . ( KnownNat n
     , RealFloat a
     , FMA a
     , HasSpectrumSamples n a
     )
  => a
  -> SampledWavelengths n a
sampleVisibleWavelengths ξ =
  SampledWavelengths (mapSamples visibleWavelengthsPdf λ) λ
  where
    λ = unfoldrSamples step 0
    step i =
      let (_ :: Integer, ξ') = properFraction $ fma (fromInteger i) nInv ξ
       in (sampleVisible ξ', succ i)
    n = natVal $ Proxy @n
    nInv = recip $ fromInteger n

{-# SPECIALIZE sampleVisible :: Float -> Float #-}
{-# SPECIALIZE sampleVisible :: Double -> Double #-}
sampleVisible :: (Floating a) => a -> a
sampleVisible u = 538 - 138.888889 * atanh (0.85691062 - 1.82750197 * u)

{-# SPECIALIZE visibleWavelengthsPdf :: Float -> Float #-}
{-# SPECIALIZE visibleWavelengthsPdf :: Double -> Double #-}
visibleWavelengthsPdf :: (Ord a, Floating a) => a -> a
visibleWavelengthsPdf λ
  | λ < 360 || λ > 830 = 0
  | otherwise = 0.0039398042 / sqrt (cosh $ 0.0072 * (λ - 538))
