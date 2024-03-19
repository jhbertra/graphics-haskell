module Statistics.Distribution2.UniformDisk (
  UniformDiskDistribution,

  -- * Constructors
  uniformDiskDistribution,
  uniformPartialDiskDistribution,
  uniformPartialDiskDistributionE,

  -- ** Accessors
  udInnerRadius,
  udθMax,

  -- ** Sampling functions
  sampleUniformDisk,
  invSampleUniformDisk,
) where

import Data.Maybe (fromMaybe)
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

data UniformDiskDistribution = UD
  { udInnerRadius :: {-# UNPACK #-} !Double
  , udθMax :: {-# UNPACK #-} !Double
  }
  deriving (Show, Read, Eq, Ord)

uniformDiskDistribution :: UniformDiskDistribution
uniformDiskDistribution = uniformPartialDiskDistribution 0 twoPi

uniformPartialDiskDistribution :: (Real a) => a -> a -> UniformDiskDistribution
uniformPartialDiskDistribution innerRadius =
  fromMaybe (error "uniformPartialDiskDistribution: invalid parameters")
    . uniformPartialDiskDistributionE innerRadius

uniformPartialDiskDistributionE :: (Real a) => a -> a -> Maybe UniformDiskDistribution
uniformPartialDiskDistributionE
  (clamp (0, 1) . realToFrac -> innerRadius)
  (clamp (0, twoPi) . realToFrac -> θMax)
    | innerRadius == 1 || θMax == 0 = Nothing
    | otherwise = Just $ UD innerRadius θMax

instance Arbitrary UniformDiskDistribution where
  arbitrary = do
    innerRadius <- choose (0, 1 - epsilon)
    θMax <- choose (0 + epsilon, twoPi)
    pure $ UD innerRadius θMax
  shrink (UD innerRadius θMax) =
    (UD <$> shrinkTowards 0 innerRadius <*> pure θMax)
      <> (UD innerRadius <$> shrinkTowards twoPi θMax)

shrinkTowards :: Double -> Double -> [Double]
shrinkTowards target a
  | δ == 0 = []
  | nearZero δ = [target]
  | otherwise = [a + (δ * 0.5)]
  where
    δ = target - a

instance Distribution2 UniformDiskDistribution where
  cumulative2
    (UD innerRadius θMax)
    (clamp (innerRadius, 1) -> radius)
    (clamp (0, θMax) -> θ) =
      (θ * (radius * radius - innerRadius * innerRadius))
        / ((1 - innerRadius * innerRadius) * θMax)

instance ContDistr2 UniformDiskDistribution where
  newtype Marginal2_X UniformDiskDistribution = UHM_radius Double
  newtype Marginal2_Y UniformDiskDistribution = UHM_θ Double
  newtype Conditional2_X UniformDiskDistribution = UHC_radius Double
  newtype Conditional2_Y UniformDiskDistribution = UHC_θ Double
  density2 (UD innerRadius θMax) r θ
    | r < innerRadius || r > 1 || θ < 0 || θ > θMax = 0
    | otherwise = (2 * r) / (θMax * (1 - innerRadius * innerRadius))
  marginal2_X (UD innerRadius _) = UHM_radius innerRadius
  marginal2_Y (UD _ θMax) = UHM_θ θMax
  conditional2_X (UD innerRadius _) _ = UHC_radius innerRadius
  conditional2_Y (UD _ θMax) _ = UHC_θ θMax

deriving instance Show (Marginal2_X UniformDiskDistribution)
deriving instance Show (Marginal2_Y UniformDiskDistribution)
deriving instance Show (Conditional2_X UniformDiskDistribution)
deriving instance Show (Conditional2_Y UniformDiskDistribution)
deriving instance Eq (Marginal2_X UniformDiskDistribution)
deriving instance Eq (Marginal2_Y UniformDiskDistribution)
deriving instance Eq (Conditional2_X UniformDiskDistribution)
deriving instance Eq (Conditional2_Y UniformDiskDistribution)

instance Arbitrary (Marginal2_X UniformDiskDistribution) where
  arbitrary = marginal2_X <$> arbitrary

instance Arbitrary (Marginal2_Y UniformDiskDistribution) where
  arbitrary = marginal2_Y <$> arbitrary

instance Arbitrary (Conditional2_X UniformDiskDistribution) where
  arbitrary = conditional2_X <$> arbitrary <*> arbitrary

instance Arbitrary (Conditional2_Y UniformDiskDistribution) where
  arbitrary = conditional2_Y <$> arbitrary <*> arbitrary

instance Distribution (Marginal2_X UniformDiskDistribution) where
  cumulative (UHM_radius innerRadius) (clamp (innerRadius, 1) -> r) =
    (r * r - innerRadius * innerRadius) / (1 - innerRadius * innerRadius)

instance ContDistr (Marginal2_X UniformDiskDistribution) where
  density (UHM_radius innerRadius) radius
    | radius < innerRadius || radius > 1 = 0
    | otherwise = 2 * radius / (1 - innerRadius * innerRadius)
  quantile (UHM_radius innerRadius) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = sqrt $ ξ - innerRadius * innerRadius * (ξ - 1)

instance Distribution (Marginal2_Y UniformDiskDistribution) where
  cumulative (UHM_θ θMax) = (/ θMax) . clamp (0, θMax)

instance ContDistr (Marginal2_Y UniformDiskDistribution) where
  density (UHM_θ θMax) θ
    | θ < 0 || θ > θMax = 0
    | otherwise = recip θMax
  quantile (UHM_θ θMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ξ * θMax

instance Distribution (Conditional2_X UniformDiskDistribution) where
  cumulative (UHC_radius innerRadius) (clamp (innerRadius, 1) -> r) =
    (r * r - innerRadius * innerRadius) / (1 - innerRadius * innerRadius)

instance ContDistr (Conditional2_X UniformDiskDistribution) where
  density (UHC_radius innerRadius) radius
    | radius < innerRadius || radius > 1 = 0
    | otherwise = 2 * radius / (1 - innerRadius * innerRadius)
  quantile (UHC_radius innerRadius) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = sqrt $ ξ - innerRadius * innerRadius * (ξ - 1)

instance Distribution (Conditional2_Y UniformDiskDistribution) where
  cumulative (UHC_θ θMax) = (/ θMax) . clamp (0, θMax)

instance ContDistr (Conditional2_Y UniformDiskDistribution) where
  density (UHC_θ θMax) θ
    | θ < 0 || θ > θMax = 0
    | otherwise = recip θMax
  quantile (UHC_θ θMax) ξ
    | ξ < 0 || ξ > 1 = error "quantile: argument out of range"
    | otherwise = ξ * θMax

sampleUniformDisk :: (RealFrac a) => UniformDiskDistribution -> Point V2 a -> Point V2 a
sampleUniformDisk d u = realToFrac <$> P (V2 x y)
  where
    P (V2 radius θ) = sampleContinuous2_XY d $ realToFrac <$> u
    x = radius * cos θ
    y = radius * sin θ

invSampleUniformDisk :: (RealFloat a) => UniformDiskDistribution -> Point V2 a -> Point V2 a
invSampleUniformDisk d (fmap realToFrac -> P v@(V2 x y)) = realToFrac <$> u
  where
    radius = norm v
    θ = case atan2 y x of
      theta
        | theta < 0 -> theta + twoPi
        | otherwise -> theta
    u = invSampleContinuous2_XY d $ P $ V2 radius θ
