{-# LANGUAGE StrictData #-}

module Statistics.Distribution.Transform where

import Control.Lens ((^.))
import Linear
import Linear.Affine
import Statistics.Distribution
import Statistics.Distribution2

-- | Sample a distribution using the inversion method. Note that this is
--   equivalent to finding the quantile of the probability density.
sampleContinuous :: (ContDistr d) => d -> Double -> Double
sampleContinuous = quantile

-- | Transform a random variable /X/ from distribution /d/ to a random variable
--   /Y/ from distribution /e/ such that /CDF_d(X) = CDF_e(Y)/
transformDistribution :: (ContDistr d, ContDistr e) => d -> e -> Double -> Double
transformDistribution d e = quantile e . cumulative d

-- | Sample a 2-D joint probability distribution function by first sampling the
-- marginal distribution of the first dimension, then the conditional
-- distribution of the second.
sampleContinuous2_XY :: (ContDistr2 d) => d -> Point V2 Double -> Point V2 Double
sampleContinuous2_XY d u = P $ V2 x y
  where
    x = sampleContinuous (marginal2_X d) $ u ^. _x
    y = sampleContinuous (conditional2_Y d x) $ u ^. _y

-- | Sample a 2-D joint probability distribution function by first sampling the
-- marginal distribution of the second dimension, then the conditional
-- distribution of the first.
sampleContinuous2_YX :: (ContDistr2 d) => d -> Point V2 Double -> Point V2 Double
sampleContinuous2_YX d u = P $ V2 x y
  where
    y = sampleContinuous (marginal2_Y d) $ u ^. _y
    x = sampleContinuous (conditional2_X d y) $ u ^. _x
