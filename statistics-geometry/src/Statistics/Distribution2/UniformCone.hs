module Statistics.Distribution2.UniformCone (
  UniformConeDistribution,

  -- * Constructors
  uniformConeDistribution,

  -- ** Accessors
  ucCosThetaMax,

  -- ** Sampling functions
  sampleUniformCone,
  invSampleUniformCone,
) where

import Data.Data (Typeable)
import Data.Ord (clamp)
import Linear
import Linear.Affine
import Statistics.Distribution
import Statistics.Distribution2
import Statistics.Distribution2.UniformSphere (inv2Pi, twoPi)
import Test.QuickCheck

newtype UniformConeDistribution = UC
  { ucCosThetaMax :: Double
  }
  deriving (Show, Eq, Ord, Typeable)

instance Arbitrary UniformConeDistribution where
  arbitrary = UC <$> choose (-1, 1)

instance Distribution2 UniformConeDistribution where
  cumulative2 (UC cosθMax) (clamp (cosθMax, 1) -> cosθ) (clamp (0, twoPi) -> ϕ) =
    (ϕ * (cosθ - cosθMax)) / (twoPi * (1 - cosθMax))

instance ContDistr2 UniformConeDistribution where
  newtype Marginal2_X UniformConeDistribution = UCM_cosθ Double
  data Marginal2_Y UniformConeDistribution = UCM_ϕ
  newtype Conditional2_X UniformConeDistribution = UCC_cosθ Double
  data Conditional2_Y UniformConeDistribution = UCC_ϕ
  density2 (UC cosθMax) _ _ = 1 / (twoPi * (1 - cosθMax))
  marginal2_X (UC cosθMax) = UCM_cosθ cosθMax
  marginal2_Y _ = UCM_ϕ
  conditional2_X (UC cosθMax) _ = UCC_cosθ cosθMax
  conditional2_Y _ _ = UCC_ϕ

deriving instance Show (Marginal2_X UniformConeDistribution)
deriving instance Show (Marginal2_Y UniformConeDistribution)
deriving instance Show (Conditional2_X UniformConeDistribution)
deriving instance Show (Conditional2_Y UniformConeDistribution)
deriving instance Eq (Marginal2_X UniformConeDistribution)
deriving instance Eq (Marginal2_Y UniformConeDistribution)
deriving instance Eq (Conditional2_X UniformConeDistribution)
deriving instance Eq (Conditional2_Y UniformConeDistribution)

instance Arbitrary (Marginal2_X UniformConeDistribution) where
  arbitrary = UCM_cosθ <$> choose (-1, 1)

instance Arbitrary (Marginal2_Y UniformConeDistribution) where
  arbitrary = pure UCM_ϕ

instance Arbitrary (Conditional2_X UniformConeDistribution) where
  arbitrary = UCC_cosθ <$> choose (-1, 1)

instance Arbitrary (Conditional2_Y UniformConeDistribution) where
  arbitrary = pure UCC_ϕ

instance Distribution (Marginal2_X UniformConeDistribution) where
  cumulative (UCM_cosθ cosθMax) (clamp (cosθMax, 1) -> cosθ) = (cosθ - cosθMax) / (1 - cosθMax)

instance ContDistr (Marginal2_X UniformConeDistribution) where
  density (UCM_cosθ cosθMax) _ = 1 / (1 - cosθMax)
  quantile (UCM_cosθ cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = cosθMax + ξ - cosθMax * ξ
  complQuantile (UCM_cosθ cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = 1 - ξ + cosθMax * ξ

instance Distribution (Marginal2_Y UniformConeDistribution) where
  cumulative _ = clamp (0, 1) . (* inv2Pi)

instance ContDistr (Marginal2_Y UniformConeDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * ξ

instance Distribution (Conditional2_X UniformConeDistribution) where
  cumulative (UCC_cosθ cosθMax) (clamp (cosθMax, 1) -> cosθ) = (cosθ - cosθMax) / (1 - cosθMax)

instance ContDistr (Conditional2_X UniformConeDistribution) where
  density (UCC_cosθ cosθMax) _ = 1 / (1 - cosθMax)
  quantile (UCC_cosθ cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = cosθMax + ξ - cosθMax * ξ
  complQuantile (UCC_cosθ cosθMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = 1 - ξ + cosθMax * ξ

instance Distribution (Conditional2_Y UniformConeDistribution) where
  cumulative _ = clamp (0, 1) . (* inv2Pi)

instance ContDistr (Conditional2_Y UniformConeDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * ξ

uniformConeDistribution :: Double -> UniformConeDistribution
uniformConeDistribution = UC . clamp (-1, 1)

sampleUniformCone :: (RealFloat a) => a -> Point V2 a -> V3 a
sampleUniformCone cosθMax u = realToFrac <$> V3 (sinθ * cos ϕ) (sinθ * sin ϕ) cosθ
  where
    d = uniformConeDistribution $ realToFrac cosθMax
    P (V2 cosθ ϕ) = sampleContinuous2_XY d $ realToFrac <$> u
    sinθ = sqrt $ 1 - cosθ * cosθ

invSampleUniformCone :: (RealFloat a, Epsilon a) => a -> V3 a -> Point V2 a
invSampleUniformCone cosθMax (fmap realToFrac . normalize -> V3 x y z) = realToFrac <$> u
  where
    cosθ = z
    ϕ = case atan2 y x of
      phi
        | phi < 0 -> phi + twoPi
        | otherwise -> phi
    d = uniformConeDistribution $ realToFrac cosθMax
    u = invSampleContinuous2_XY d $ P $ V2 cosθ ϕ
