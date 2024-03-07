-- |
-- Module    : Statistics.Distribution.Linear
-- Copyright : (c) 2024 Jamie Bertram
-- License   : MIT
--
-- Maintainer  : Jamie Bertram
-- Stability   : Experimental
-- Portability : portable
--
-- Distribution within a finite interval proportional to a linear equation.
module Statistics.Distribution.Linear (
  LinearDistribution,

  -- * Constructors
  linearDistributionE,
  linearDistribution,

  -- ** Accessors
  linearA,
  linearU,
  linearB,
  linearV,
  linearNorm,
) where

import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import GHC.Stack (HasCallStack)
import Statistics.Distribution (
  ContDistr (..),
  ContGen (..),
  Distribution (..),
  MaybeMean (..),
  MaybeVariance (..),
  Mean (..),
  Variance (..),
  genContinuous,
 )
import Test.QuickCheck

-- | Proportional linear distribution from (A, U) to (B, V).
data LinearDistribution = LinearDistribution
  { linearA :: {-# UNPACK #-} !Double
  -- ^ Lower Boundary of distribution
  , linearU :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at A
  , linearB :: {-# UNPACK #-} !Double
  -- ^ Upper Boundary of distribution
  , linearV :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at B
  , linearNorm :: {-# UNPACK #-} !Double
  -- ^ Normalization factor. Used to ensure the resulting PDF integrates to 1
  -- over its domain.
  --
  -- This is derived from the other parameters, and is pre-computed for
  -- efficiency:
  --
  -- > linearNorm = 2 / ((linearB - linearA) * (linearU + linearV))
  }
  deriving (Show, Eq, Ord, Typeable)

instance Distribution LinearDistribution where
  cumulative (LinearDistribution a u b v _) x
    | x <= a = 0
    | x >= b = 1
    | u == v = (a - x) / (a - b)
    | otherwise =
        clamp (0, 1) $
          ((a - x) * ((u - v) * x + (u + v) * a - 2 * u * b)) / ((a - b) * (a - b) * (u + v))

instance ContDistr LinearDistribution where
  density (LinearDistribution a u b v norm) x
    | x < a || x > b = 0
    | x == a = u * norm
    | x == b = v * norm
    | u == v = norm
    | otherwise =
        let t = (x - a) / (b - a)
         in max 0 $ norm * ((1 - t) * u + t * v)
  quantile (LinearDistribution a u b v _) x
    | x < 0 = error "Statistics.Distribution.Linear.quantile: negative probability"
    | x > 1 = error "Statistics.Distribution.Linear.quantile: probability > 1"
    | x == 0 = a
    | x == 1 = b
    | u == v = x * (b - a) + a
    | otherwise =
        ((a - b) * sqrt (v * v * x - u * u * (x - 1)) - a * v + b * u) / (u - v)
  complQuantile (LinearDistribution a u b v _) x
    | x < 0 = error "Statistics.Distribution.Linear.complQuantile: probability > 1"
    | x > 1 = error "Statistics.Distribution.Linear.complQuantile: negative probability"
    | x == 0 = b
    | x == 1 = a
    | u == v = x * (a - b) + b
    | otherwise =
        ((a - b) * sqrt (u * u * x + v * v * (1 - x)) - a * v + b * u) / (u - v)

instance MaybeMean LinearDistribution where
  maybeMean = Just . mean

instance Mean LinearDistribution where
  mean (LinearDistribution a u b v _) =
    (a * (2 * u + v) + b * (2 * v + u)) / (3 * (u + v))

instance MaybeVariance LinearDistribution where
  maybeVariance = Just . variance

instance Variance LinearDistribution where
  variance (LinearDistribution a u b v _) =
    ((a - b) * (a - b) * (a - b) * (u * u + u * v + v * v))
      / (9 * (u + v) * (u + v))

instance ContGen LinearDistribution where
  genContVar = genContinuous

-- | Create a linear distribution from (a, u) to (b, v).
--
-- > linearDistribution a u b v
--
-- Throws a runtime error if the parameters are invalid.
linearDistribution :: (HasCallStack) => Double -> Double -> Double -> Double -> LinearDistribution
linearDistribution a u b v = fromMaybe (error msg) $ linearDistributionE a u b v
  where
    msg = "Statistics.Distribution.Linear.linearDistribution: invalid distribution."

-- | Create a linear distribution from (a, u) to (b, v)
--
-- > linearDistributionE a u b v
--
-- Returns Nothing if the parameters are invalid. Requires that u, v are
-- positive and do not sum to zero. Requires that a /= b.
linearDistributionE :: Double -> Double -> Double -> Double -> Maybe LinearDistribution
linearDistributionE a u b v = case compare b a of
  LT -> linearDistributionE b v a u
  EQ -> Nothing
  GT
    | u < 0 || v < 0 || u + v == 0 -> Nothing
    | otherwise -> Just $ LinearDistribution a u b v $ 2 / ((b - a) * (u + v))

instance Arbitrary LinearDistribution where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    u <- abs <$> arbitrary
    v <- abs <$> arbitrary `suchThat` ((/= 0) . (u +))
    pure $ linearDistribution a u b v
