module Statistics.Distribution2.UniformSphere (
  UniformSphereDistribution,

  -- * Constructors
  uniformHemisphereDistribution,
  uniformSphereDistribution,
  uniformPartialSphereDistribution,

  -- ** Accessors
  usCosθMax,
  usCosθMin,
  usϕMax,

  -- ** Sampling functions
  sampleUniformSphere,
  invSampleUniformSphere,
) where

import Data.Ord (clamp)
import Linear
import Linear.Affine
import Numeric.IEEE (IEEE (..))
import Statistics.Distribution
import Statistics.Distribution2
import Test.QuickCheck (Arbitrary, choose)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

twoPi :: Double
twoPi = pi * 2

data UniformSphereDistribution = US
  { usCosθMin :: {-# UNPACK #-} !Double
  , usCosθMax :: {-# UNPACK #-} !Double
  , usϕMax :: {-# UNPACK #-} !Double
  }
  deriving (Show, Read, Eq, Ord)

uniformHemisphereDistribution :: UniformSphereDistribution
uniformHemisphereDistribution = uniformPartialSphereDistribution 1 0 twoPi

uniformSphereDistribution :: UniformSphereDistribution
uniformSphereDistribution = uniformPartialSphereDistribution 1 (-1) twoPi

uniformPartialSphereDistribution :: (Real a) => a -> a -> a -> UniformSphereDistribution
uniformPartialSphereDistribution
  (clamp (-1, 1) . realToFrac -> cosθMin)
  (clamp (-1, 1) . realToFrac -> cosθMax)
  (clamp (0, twoPi) . realToFrac -> ϕMax) = case compare cosθMin cosθMax of
    LT -> uniformPartialSphereDistribution cosθMax cosθMin ϕMax
    EQ -> error "uniformPartialSphereDistribution: invalid parameters"
    GT -> US cosθMin cosθMax ϕMax

instance Arbitrary UniformSphereDistribution where
  arbitrary = do
    cosθMax <- choose (0, predIEEE 1)
    cosθMin <- choose (succIEEE cosθMax, 1)
    ϕMax <- choose (0, twoPi)
    pure $ US cosθMin cosθMax ϕMax
  shrink (US cosθMin cosθMax ϕMax) =
    (US <$> shrinkTowards 1 cosθMin <*> pure cosθMax <*> pure ϕMax)
      <> (US cosθMin <$> shrinkTowards 0 cosθMax <*> pure ϕMax)
      <> (US cosθMin cosθMax <$> shrinkTowards twoPi ϕMax)

shrinkTowards :: Double -> Double -> [Double]
shrinkTowards target a
  | δ == 0 = []
  | nearZero δ = [target]
  | otherwise = [a + (δ * 0.5)]
  where
    δ = target - a

instance Distribution2 UniformSphereDistribution where
  cumulative2
    (US cosθMin cosθMax ϕMax)
    (clamp (cosθMax, cosθMin) -> cosθ)
    (clamp (0, ϕMax) -> ϕ) = (ϕ * (cosθ - cosθMax)) / (ϕMax * (cosθMin - cosθMax))

instance ContDistr2 UniformSphereDistribution where
  data Marginal2_X UniformSphereDistribution
    = UHM_cosθ
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
  newtype Marginal2_Y UniformSphereDistribution = UHM_ϕ Double
  data Conditional2_X UniformSphereDistribution
    = UHC_cosθ
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
  newtype Conditional2_Y UniformSphereDistribution = UHC_ϕ Double
  density2 (US cosθMin cosθMax ϕMax) cosθ ϕ
    | cosθ < cosθMin || cosθ > cosθMax || ϕ < 0 || ϕ > ϕMax = 0
    | otherwise = recip $ ϕMax * (cosθMin - cosθMax)
  marginal2_X (US cosθMin cosθMax _) = UHM_cosθ cosθMin cosθMax
  marginal2_Y (US _ _ ϕMax) = UHM_ϕ ϕMax
  conditional2_X (US cosθMin cosθMax _) _ = UHC_cosθ cosθMin cosθMax
  conditional2_Y (US _ _ ϕMax) _ = UHC_ϕ ϕMax

deriving instance Show (Marginal2_X UniformSphereDistribution)
deriving instance Show (Marginal2_Y UniformSphereDistribution)
deriving instance Show (Conditional2_X UniformSphereDistribution)
deriving instance Show (Conditional2_Y UniformSphereDistribution)
deriving instance Eq (Marginal2_X UniformSphereDistribution)
deriving instance Eq (Marginal2_Y UniformSphereDistribution)
deriving instance Eq (Conditional2_X UniformSphereDistribution)
deriving instance Eq (Conditional2_Y UniformSphereDistribution)

instance Arbitrary (Marginal2_X UniformSphereDistribution) where
  arbitrary = marginal2_X <$> arbitrary

instance Arbitrary (Marginal2_Y UniformSphereDistribution) where
  arbitrary = marginal2_Y <$> arbitrary

instance Arbitrary (Conditional2_X UniformSphereDistribution) where
  arbitrary = conditional2_X <$> arbitrary <*> arbitrary

instance Arbitrary (Conditional2_Y UniformSphereDistribution) where
  arbitrary = conditional2_Y <$> arbitrary <*> arbitrary

instance Distribution (Marginal2_X UniformSphereDistribution) where
  cumulative (UHM_cosθ cosθMin cosθMax) =
    (/ (cosθMin - cosθMax)) . subtract cosθMax . clamp (cosθMax, cosθMin)

instance ContDistr (Marginal2_X UniformSphereDistribution) where
  density (UHM_cosθ cosθMin cosθMax) cosθ
    | cosθ < cosθMax || cosθ > cosθMin = 0
    | otherwise = 1 / (cosθMin - cosθMax)
  quantile (UHM_cosθ cosθMin cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ξ * (cosθMin - cosθMax) + cosθMax

instance Distribution (Marginal2_Y UniformSphereDistribution) where
  cumulative (UHM_ϕ ϕMax) = (/ ϕMax) . clamp (0, ϕMax)

instance ContDistr (Marginal2_Y UniformSphereDistribution) where
  density (UHM_ϕ ϕMax) ϕ
    | ϕ < 0 || ϕ > ϕMax = 0
    | otherwise = recip ϕMax
  quantile (UHM_ϕ ϕMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ϕMax * ξ

instance Distribution (Conditional2_X UniformSphereDistribution) where
  cumulative (UHC_cosθ cosθMin cosθMax) =
    (/ (cosθMin - cosθMax)) . subtract cosθMax . clamp (cosθMax, cosθMin)

instance ContDistr (Conditional2_X UniformSphereDistribution) where
  density (UHC_cosθ cosθMin cosθMax) cosθ
    | cosθ < cosθMax || cosθ > cosθMin = 0
    | otherwise = 1 / (cosθMin - cosθMax)
  quantile (UHC_cosθ cosθMin cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ξ * (cosθMin - cosθMax) + cosθMax

instance Distribution (Conditional2_Y UniformSphereDistribution) where
  cumulative (UHC_ϕ ϕMax) = (/ ϕMax) . clamp (0, ϕMax)

instance ContDistr (Conditional2_Y UniformSphereDistribution) where
  density (UHC_ϕ ϕMax) ϕ
    | ϕ < 0 || ϕ > ϕMax = 0
    | otherwise = recip ϕMax
  quantile (UHC_ϕ ϕMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ϕMax * ξ

sampleUniformSphere :: (RealFrac a) => UniformSphereDistribution -> Point V2 a -> V3 a
sampleUniformSphere d u = realToFrac <$> V3 x y z
  where
    P (V2 cosθ ϕ) = sampleContinuous2_XY d $ realToFrac <$> u
    sinθ = sqrt $ 1 - cosθ * cosθ
    x = sinθ * cos ϕ
    y = sinθ * sin ϕ
    z = cosθ

invSampleUniformSphere :: (RealFloat a, Epsilon a) => UniformSphereDistribution -> V3 a -> Point V2 a
invSampleUniformSphere d (fmap realToFrac . normalize -> V3 x y z) = realToFrac <$> u
  where
    cosθ = z
    ϕ = case atan2 y x of
      phi
        | phi < 0 -> phi + twoPi
        | otherwise -> phi
    u = invSampleContinuous2_XY d $ P $ V2 cosθ ϕ
