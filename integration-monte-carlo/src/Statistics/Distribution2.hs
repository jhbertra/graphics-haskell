module Statistics.Distribution2 where

import Data.Kind (Type)
import Linear
import Linear.Affine
import Statistics.Distribution (ContDistr (..), genContinuous)
import Statistics.Function (square)
import System.Random.Stateful

-- | Type class for 2-dimensional joint distributions.
class Distribution2 d where
  -- | 2-D cumulative distribution function.  The joint probability that
  -- random variables /X/ and /Y/ are less or equal than /x/ and /y/,
  -- i.e. P(/X/≤/x/, /Y/≤/y/). Cumulative should be defined for
  -- infinities as well:
  --
  -- > cumulative d +∞ +∞ = 1
  -- > cumulative d -∞ -∞ = 0
  cumulative2 :: d -> Double -> Double -> Double
  cumulative2 d x y = 1 - complCumulative2 d x y

  -- | One's complement of 2-D cumulative distribution:
  --
  -- > complCumulative2 d x y = 1 - cumulative2 d x y
  --
  -- It's useful when one is interested in P(/X/>/x/, /Y/>/y/) and
  -- the expression on the right side begin to lose precision. This
  -- function have default implementation but implementors are
  -- encouraged to provide more precise implementation.
  complCumulative2 :: d -> Double -> Double -> Double
  complCumulative2 d x y = 1 - cumulative2 d x y

  {-# MINIMAL (cumulative2 | complCumulative2) #-}

-- | Continuous 2-D join probability distributions.
class
  ( ContDistr (Marginal2_X d)
  , ContDistr (Marginal2_Y d)
  , ContDistr (Conditional2_X d)
  , ContDistr (Conditional2_Y d)
  , Distribution2 d
  ) =>
  ContDistr2 d
  where
  -- | An associated data type that defines the marginal distribution of values
  -- from the x dimension
  data Marginal2_X d :: Type

  -- | An associated data type that defines the conditional distribution of values
  -- from the x dimension
  data Conditional2_X d :: Type

  -- | An associated data type that defines the marginal distribution of values
  -- from the y dimension
  data Marginal2_Y d :: Type

  -- | An associated data type that defines the conditional distribution of values
  -- from the y dimension
  data Conditional2_Y d :: Type

  -- | Probability density function. Probability that random
  -- variables /X/ /Y/ lie in the infinitesimal area
  -- [/x/,/x+/δ/x/)✕[/y/,/y+/δ/y/) equal to /density(x, y)/⋅δ/x/⋅δ/y/
  density2 :: d -> Double -> Double -> Double
  density2 d x y = exp $ logDensity2 d x y

  -- | Natural logarithm of density.
  logDensity2 :: d -> Double -> Double -> Double
  logDensity2 d x y = log $ density2 d x y

  -- | Compute the marginal distribution of values from the x dimension
  -- over the average probability for all values from the y.
  --
  -- The definition with respect to /density2` is
  --
  -- > density (marginal2_X d) x = ∫ density2 d x y dy
  marginal2_X :: d -> Marginal2_X d

  -- | Compute the conditional distribution of values from the x dimension
  -- given a fixed value from the y.
  --
  -- The definition with respect to /density2` should satisfy:
  --
  -- > density (conditional2_X d y) x = density2 d x y / density (marginal2_Y d) y
  conditional2_X :: d -> Double -> Conditional2_X d

  -- | Compute the marginal distribution of values from the y dimension
  -- over the average probability for all values from the x.
  --
  -- The definition with respect to /density2` is
  --
  -- > density (marginal2_Y d y) = ∫ density2 d x y dx
  marginal2_Y :: d -> Marginal2_Y d

  -- | Compute the conditional distribution of values from the y dimension
  -- given a fixed value from the x.
  --
  -- The definition with respect to /density2` should satisfy:
  --
  -- > density (conditional2_Y d x) y = density2 d x y / density (marginal2_X d) x
  conditional2_Y :: d -> Double -> Conditional2_Y d

  {-# MINIMAL (density2 | logDensity2), marginal2_X, conditional2_X, marginal2_Y, conditional2_Y #-}

class (Distribution2 d) => MaybeMean2 d where
  maybeMean2 :: d -> Maybe Double

class (MaybeMean2 d) => Mean2 d where
  mean2 :: d -> Double

class (MaybeMean2 d) => MaybeVariance2 d where
  maybeVariance2 :: d -> Maybe Double
  maybeVariance2 = fmap square . maybeStdDev2

  maybeStdDev2 :: d -> Maybe Double
  maybeStdDev2 = fmap sqrt . maybeVariance2
  {-# MINIMAL (maybeVariance2 | maybeStdDev2) #-}

class (Mean2 d, MaybeVariance2 d) => Variance d where
  variance2 :: d -> Double
  variance2 = square . variance2

  stdDev2 :: d -> Double
  stdDev2 = sqrt . variance2
  {-# MINIMAL (variance2 | stdDev2) #-}

class (Distribution2 d) => MaybeEntropy2 d where
  maybeEntropy2 :: d -> Maybe Double

class (MaybeEntropy2 d) => Entropy2 d where
  entropy2 :: d -> Double

class (Distribution2 d) => ContGen2 d where
  genContVar2 :: (StatefulGen g m) => d -> g -> m (Point V2 Double)

genContinuous2 :: (StatefulGen g m, ContDistr2 d) => d -> g -> m (Point V2 Double)
genContinuous2 d g = do
  x <- genContinuous (marginal2_X d) g
  y <- genContinuous (conditional2_Y d x) g
  pure $! P $ V2 x y
