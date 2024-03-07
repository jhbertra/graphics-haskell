module Statistics.Distribution2.UniformSphere where

import Data.Ord (clamp)
import Linear
import Linear.Affine
import Statistics.Distribution
import Statistics.Distribution2
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

inv2Pi :: Double
inv2Pi = 1 / (2 * pi)

inv4Pi :: Double
inv4Pi = 1 / (4 * pi)

halfPi :: Double
halfPi = pi / 2

twoPi :: Double
twoPi = pi * 2

data UniformHemisphereDistribution = UniformHemisphereDistribution
  deriving (Show, Read, Eq, Ord)

instance Arbitrary UniformHemisphereDistribution where
  arbitrary = pure UniformHemisphereDistribution

instance Distribution2 UniformHemisphereDistribution where
  cumulative2 _ (clamp (0, 1) -> cosθ) (clamp (0, twoPi) -> ϕ) = cosθ * ϕ * inv2Pi

instance ContDistr2 UniformHemisphereDistribution where
  data Marginal2_X UniformHemisphereDistribution = UniformHemisphereMarginalThetaDistribution
  data Marginal2_Y UniformHemisphereDistribution = UniformHemisphereMarginalPhiDistribution
  data Conditional2_X UniformHemisphereDistribution = UniformHemisphereConditionalThetaDistribution
  data Conditional2_Y UniformHemisphereDistribution = UniformHemisphereConditionalPhiDistribution
  density2 _ cosθ ϕ
    | cosθ < 0 || cosθ > 1 = 0
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  marginal2_X _ = UniformHemisphereMarginalThetaDistribution
  marginal2_Y _ = UniformHemisphereMarginalPhiDistribution
  conditional2_X _ _ = UniformHemisphereConditionalThetaDistribution
  conditional2_Y _ _ = UniformHemisphereConditionalPhiDistribution

deriving instance Show (Marginal2_X UniformHemisphereDistribution)
deriving instance Show (Marginal2_Y UniformHemisphereDistribution)
deriving instance Show (Conditional2_X UniformHemisphereDistribution)
deriving instance Show (Conditional2_Y UniformHemisphereDistribution)
deriving instance Eq (Marginal2_X UniformHemisphereDistribution)
deriving instance Eq (Marginal2_Y UniformHemisphereDistribution)
deriving instance Eq (Conditional2_X UniformHemisphereDistribution)
deriving instance Eq (Conditional2_Y UniformHemisphereDistribution)

instance Arbitrary (Marginal2_X UniformHemisphereDistribution) where
  arbitrary = pure UniformHemisphereMarginalThetaDistribution

instance Arbitrary (Marginal2_Y UniformHemisphereDistribution) where
  arbitrary = pure UniformHemisphereMarginalPhiDistribution

instance Arbitrary (Conditional2_X UniformHemisphereDistribution) where
  arbitrary = pure UniformHemisphereConditionalThetaDistribution

instance Arbitrary (Conditional2_Y UniformHemisphereDistribution) where
  arbitrary = pure UniformHemisphereConditionalPhiDistribution

instance Distribution (Marginal2_X UniformHemisphereDistribution) where
  cumulative _ = clamp (0, 1)

instance ContDistr (Marginal2_X UniformHemisphereDistribution) where
  density _ cosθ
    | cosθ < 0 || cosθ > 1 = 0
    | otherwise = 1
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = x

instance Distribution (Marginal2_Y UniformHemisphereDistribution) where
  cumulative _ = (* inv2Pi) . clamp (0, twoPi)

instance ContDistr (Marginal2_Y UniformHemisphereDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * x

instance Distribution (Conditional2_X UniformHemisphereDistribution) where
  cumulative _ = clamp (0, 1)

instance ContDistr (Conditional2_X UniformHemisphereDistribution) where
  density _ cosθ
    | cosθ < 0 || cosθ > 1 = 0
    | otherwise = 1
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = x

instance Distribution (Conditional2_Y UniformHemisphereDistribution) where
  cumulative _ = (* inv2Pi) . clamp (0, twoPi)

instance ContDistr (Conditional2_Y UniformHemisphereDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * x

data UniformSphereDistribution = UniformSphereDistribution
  deriving (Show, Read, Eq, Ord)

instance Arbitrary UniformSphereDistribution where
  arbitrary = pure UniformSphereDistribution

instance Distribution2 UniformSphereDistribution where
  cumulative2 _ (clamp (-1, 1) -> cosθ) (clamp (0, twoPi) -> ϕ) = (cosθ + 1) * ϕ * inv4Pi

instance ContDistr2 UniformSphereDistribution where
  data Marginal2_X UniformSphereDistribution = UniformSphereMarginalThetaDistribution
  data Marginal2_Y UniformSphereDistribution = UniformSphereMarginalPhiDistribution
  data Conditional2_X UniformSphereDistribution = UniformSphereConditionalThetaDistribution
  data Conditional2_Y UniformSphereDistribution = UniformSphereConditionalPhiDistribution
  density2 _ cosθ ϕ
    | cosθ < -1 || cosθ > 1 = 0
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv4Pi
  marginal2_X _ = UniformSphereMarginalThetaDistribution
  marginal2_Y _ = UniformSphereMarginalPhiDistribution
  conditional2_X _ _ = UniformSphereConditionalThetaDistribution
  conditional2_Y _ _ = UniformSphereConditionalPhiDistribution

deriving instance Show (Marginal2_X UniformSphereDistribution)
deriving instance Show (Marginal2_Y UniformSphereDistribution)
deriving instance Show (Conditional2_X UniformSphereDistribution)
deriving instance Show (Conditional2_Y UniformSphereDistribution)
deriving instance Eq (Marginal2_X UniformSphereDistribution)
deriving instance Eq (Marginal2_Y UniformSphereDistribution)
deriving instance Eq (Conditional2_X UniformSphereDistribution)
deriving instance Eq (Conditional2_Y UniformSphereDistribution)

instance Arbitrary (Marginal2_X UniformSphereDistribution) where
  arbitrary = pure UniformSphereMarginalThetaDistribution

instance Arbitrary (Marginal2_Y UniformSphereDistribution) where
  arbitrary = pure UniformSphereMarginalPhiDistribution

instance Arbitrary (Conditional2_X UniformSphereDistribution) where
  arbitrary = pure UniformSphereConditionalThetaDistribution

instance Arbitrary (Conditional2_Y UniformSphereDistribution) where
  arbitrary = pure UniformSphereConditionalPhiDistribution

instance Distribution (Marginal2_X UniformSphereDistribution) where
  cumulative _ (clamp (-1, 1) -> cosθ) = (cosθ + 1) / 2

instance ContDistr (Marginal2_X UniformSphereDistribution) where
  density _ cosθ
    | cosθ < -1 || cosθ > 1 = 0
    | otherwise = 0.5
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = 2 * x - 1

instance Distribution (Marginal2_Y UniformSphereDistribution) where
  cumulative _ = (* inv2Pi) . clamp (0, twoPi)

instance ContDistr (Marginal2_Y UniformSphereDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * x

instance Distribution (Conditional2_X UniformSphereDistribution) where
  cumulative _ (clamp (-1, 1) -> cosθ) = (cosθ + 1) / 2

instance ContDistr (Conditional2_X UniformSphereDistribution) where
  density _ cosθ
    | cosθ < -1 || cosθ > 1 = 0
    | otherwise = 0.5
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = 2 * x - 1

instance Distribution (Conditional2_Y UniformSphereDistribution) where
  cumulative _ = (* inv2Pi) . clamp (0, twoPi)

instance ContDistr (Conditional2_Y UniformSphereDistribution) where
  density _ ϕ
    | ϕ < 0 || ϕ > twoPi = 0
    | otherwise = inv2Pi
  quantile _ x
    | x < 0 || x > 1 = error "quantile: argument out of range"
    | otherwise = twoPi * x

sampleUniformHemisphere :: (RealFrac a) => Point V2 a -> V3 a
sampleUniformHemisphere u = realToFrac <$> V3 x y z
  where
    P (V2 cosθ ϕ) = sampleContinuous2_XY UniformHemisphereDistribution $ realToFrac <$> u
    sinθ = sqrt $ 1 - cosθ * cosθ
    x = sinθ * cos ϕ
    y = sinθ * sin ϕ
    z = cosθ

invSampleUniformHemisphere :: (RealFloat a, Epsilon a) => V3 a -> Point V2 a
invSampleUniformHemisphere (fmap realToFrac . normalize -> V3 x y z) = realToFrac <$> u
  where
    cosθ = z
    ϕ = case atan2 y x of
      phi
        | phi < 0 -> phi + twoPi
        | otherwise -> phi
    u = invSampleContinuous2_XY UniformHemisphereDistribution $ P $ V2 cosθ ϕ

sampleUniformSphere :: (RealFrac a) => Point V2 a -> V3 a
sampleUniformSphere u = realToFrac <$> V3 x y z
  where
    P (V2 cosθ ϕ) = sampleContinuous2_XY UniformSphereDistribution $ realToFrac <$> u
    sinθ = sqrt $ 1 - cosθ * cosθ
    x = sinθ * cos ϕ
    y = sinθ * sin ϕ
    z = cosθ

invSampleUniformSphere :: (RealFloat a, Epsilon a) => V3 a -> Point V2 a
invSampleUniformSphere (fmap realToFrac . normalize -> V3 x y z) = realToFrac <$> u
  where
    cosθ = z
    ϕ = case atan2 y x of
      phi
        | phi < 0 -> phi + twoPi
        | otherwise -> phi
    u = invSampleContinuous2_XY UniformSphereDistribution $ P $ V2 cosθ ϕ
