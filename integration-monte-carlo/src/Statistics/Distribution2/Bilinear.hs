-- |
-- Module    : Statistics.Distribution2.Bilinear
-- Copyright : (c) 2024 Jamie Bertram
-- License   : MIT
--
-- Maintainer  : Jamie Bertram
-- Stability   : Experimental
-- Portability : portable
--
-- Distribution within a finite rectilinear area proportional to a bilinear interpolation function.
module Statistics.Distribution2.Bilinear (
  BilinearDistribution,

  -- * Constructors
  bilinearDistributionE,
  bilinearDistribution,

  -- ** Accessors
  bilinearA,
  bilinearAx,
  bilinearAy,
  bilinearB,
  bilinearBx,
  bilinearBy,
  bilinearS,
  bilinearT,
  bilinearU,
  bilinearV,
  bilinearNorm,
) where

import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Linear hiding (norm)
import Linear.Affine
import Statistics.Distribution (ContDistr, ContGen, Distribution, MaybeMean, MaybeVariance, Mean, Variance)
import Statistics.Distribution.Linear
import Statistics.Distribution2 (ContDistr2 (..), Distribution2 (..))

-- | Proportional bilinear distribution over the rectangle of values
-- (Ax, Ay, S), (Bx, Ay, T), (Ax, By, U), (Bx, By, V)
data BilinearDistribution = BilinearDistribution
  { bilinearAx :: {-# UNPACK #-} !Double
  -- ^ X-coordinate of lower Boundary of distribution
  , bilinearAy :: {-# UNPACK #-} !Double
  -- ^ Y-coordinate of lower Boundary of distribution
  , bilinearBx :: {-# UNPACK #-} !Double
  -- ^ X-coordinate of upper Boundary of distribution
  , bilinearBy :: {-# UNPACK #-} !Double
  -- ^ Y-coordinate of upper Boundary of distribution
  , bilinearS :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at (Ax, Ay)
  , bilinearT :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at (Bx, Ay)
  , bilinearU :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at (Ax, By)
  , bilinearV :: {-# UNPACK #-} !Double
  -- ^ Relative probability density at (Bx, By)
  , bilinearNorm :: {-# UNPACK #-} !Double
  -- ^ Normalization factor. Used to ensure the resulting PDF integrates to 1
  -- over its domain.
  --
  -- This is derived from the other parameters, and is pre-computed for
  -- efficiency:
  --
  -- > linearNorm = 2 / ((linearB - linearA) * (linearU + linearV))
  }
  deriving (Show, Eq, Ord, Typeable)

instance Distribution2 BilinearDistribution where
  cumulative2 (BilinearDistribution ax ay bx by s t u v _) x y
    | x <= ax || y <= ay = 0
    | x >= bx || y >= by = 1
    | otherwise =
        clamp (0, 1) $
          product
            [ x - ax
            , y - ay
            , sum
                [ sum [(s - t - u + v) * x, (s + t - u - v) * ax, -2 * (s - u) * bx] * y
                , sum [(s - t + u - v) * x, (s + t + u + v) * ax, -2 * (s + u) * bx] * ay
                , -2 * ((s - t) * x + ax * (s + t) - 2 * bx * s) * by
                ]
            ]
            / product
              [ ax - bx
              , ax - bx
              , ay - by
              , ay - by
              , s + t + u + v
              ]

instance ContDistr2 BilinearDistribution where
  newtype Marginal2_X BilinearDistribution = BilinearMarginalX LinearDistribution
  newtype Marginal2_Y BilinearDistribution = BilinearMarginalY LinearDistribution
  newtype Conditional2_X BilinearDistribution = BilinearConditionalX LinearDistribution
  newtype Conditional2_Y BilinearDistribution = BilinearConditionalY LinearDistribution

  density2 (BilinearDistribution ax ay bx by s t u v norm) x y
    | x < ax || y < ay || x > bx || y > by = 0
    | s == t && t == u && u == v = norm
    | otherwise =
        let x' = (x - ax) / (bx - ax)
            y' = (y - ay) / (by - ay)
         in max 0 $
              norm
                * sum
                  [ (1 - x') * (1 - y') * s
                  , x' * (1 - y') * t
                  , (1 - x') * y' * u
                  , x' * y' * v
                  ]

  marginal2_X (BilinearDistribution ax _ bx _ s t u v _) =
    BilinearMarginalX $ linearDistribution ax (s + u) bx (t + v)

  marginal2_Y (BilinearDistribution _ ay _ by s t u v _) =
    BilinearMarginalY $ linearDistribution ay (s + t) by (u + v)

  conditional2_X (BilinearDistribution ax ay bx by s t u v _) y =
    BilinearConditionalX $
      linearDistribution
        ax
        ((1 - y') * s + y' * u)
        bx
        ((1 - y') * t + y' * v)
    where
      y' = (y - ay) / (by - ay)

  conditional2_Y (BilinearDistribution ax ay bx by s t u v _) x =
    BilinearConditionalY $
      linearDistribution
        ay
        ((1 - x') * s + x' * t)
        by
        ((1 - x') * u + x' * v)
    where
      x' = (x - ax) / (bx - ax)

deriving newtype instance Distribution (Marginal2_X BilinearDistribution)
deriving newtype instance Distribution (Marginal2_Y BilinearDistribution)
deriving newtype instance Distribution (Conditional2_X BilinearDistribution)
deriving newtype instance Distribution (Conditional2_Y BilinearDistribution)
deriving newtype instance ContDistr (Marginal2_X BilinearDistribution)
deriving newtype instance ContDistr (Marginal2_Y BilinearDistribution)
deriving newtype instance ContDistr (Conditional2_X BilinearDistribution)
deriving newtype instance ContDistr (Conditional2_Y BilinearDistribution)
deriving newtype instance MaybeMean (Marginal2_X BilinearDistribution)
deriving newtype instance MaybeMean (Marginal2_Y BilinearDistribution)
deriving newtype instance MaybeMean (Conditional2_X BilinearDistribution)
deriving newtype instance MaybeMean (Conditional2_Y BilinearDistribution)
deriving newtype instance Mean (Marginal2_X BilinearDistribution)
deriving newtype instance Mean (Marginal2_Y BilinearDistribution)
deriving newtype instance Mean (Conditional2_X BilinearDistribution)
deriving newtype instance Mean (Conditional2_Y BilinearDistribution)
deriving newtype instance MaybeVariance (Marginal2_X BilinearDistribution)
deriving newtype instance MaybeVariance (Marginal2_Y BilinearDistribution)
deriving newtype instance MaybeVariance (Conditional2_X BilinearDistribution)
deriving newtype instance MaybeVariance (Conditional2_Y BilinearDistribution)
deriving newtype instance Variance (Marginal2_X BilinearDistribution)
deriving newtype instance Variance (Marginal2_Y BilinearDistribution)
deriving newtype instance Variance (Conditional2_X BilinearDistribution)
deriving newtype instance Variance (Conditional2_Y BilinearDistribution)
deriving newtype instance ContGen (Marginal2_X BilinearDistribution)
deriving newtype instance ContGen (Marginal2_Y BilinearDistribution)
deriving newtype instance ContGen (Conditional2_X BilinearDistribution)
deriving newtype instance ContGen (Conditional2_Y BilinearDistribution)

bilinearA :: BilinearDistribution -> Point V2 Double
bilinearA BilinearDistribution{..} = P $ V2 bilinearAx bilinearAy

bilinearB :: BilinearDistribution -> Point V2 Double
bilinearB BilinearDistribution{..} = P $ V2 bilinearBx bilinearBy

-- | Create a bilinear distribution over
-- (Ax, Ay, S), (Bx, Ay, T), (Ax, By, U), (Bx, By, V)
--
-- > bilinearDistribution ax ay bx by s t u v
--
-- Throws a runtime exception if the parameters are invalid.
-- Requires that s, t, u, v are positive and do not sum to zero.
-- Requires that ax /= bx and ay /= by.
bilinearDistribution
  :: Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> BilinearDistribution
bilinearDistribution ax ay bx by s t u v =
  fromMaybe (error msg) $ bilinearDistributionE ax ay bx by s t u v
  where
    msg = "Statistics.Distribution2.Bilinear.bilinearDistribution: invalid distribution."

-- | Create a bilinear distribution over
-- (Ax, Ay, S), (Bx, Ay, T), (Ax, By, U), (Bx, By, V)
--
-- > bilinearDistributionE ax ay bx by s t u v
--
-- Returns Nothing if the parameters are invalid.
-- Requires that s, t, u, v are positive and do not sum to zero.
-- Requires that ax /= bx and ay /= by.
bilinearDistributionE
  :: Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Maybe BilinearDistribution
bilinearDistributionE ax ay bx by s t u v = case compare bx ax of
  LT -> bilinearDistributionE bx ay ax by t s v u
  EQ -> Nothing
  GT -> case compare by ay of
    LT -> bilinearDistributionE ax by bx ay u v s t
    EQ -> Nothing
    GT
      | s < 0 || t < 0 || u < 0 || v < 0 || s + t + u + v == 0 -> Nothing
      | otherwise ->
          Just $
            BilinearDistribution ax ay bx by s t u v $
              4 / ((ax - bx) * (ay - by) * (s + t + u + v))
