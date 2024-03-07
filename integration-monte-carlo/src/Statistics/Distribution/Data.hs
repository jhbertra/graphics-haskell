module Statistics.Distribution.Data where

import Data.Kind (Type)

class (Floating a) => Distribution d a | d -> a where
  type Domain d :: Type

  cumulative :: d -> Domain d -> a
  cumulative d domain = 1 - complCumulative d domain

  complCumulative :: d -> Domain d -> a
  complCumulative d domain = 1 - cumulative d domain

  {-# MINIMAL (cumulative | complCumulative) #-}

class (Distribution d a) => ContDistr d a | d -> a where
  type SampleDomain d :: Type

  density :: d -> Domain d -> a
  density d = exp . logDensity d

  logDensity :: d -> Domain d -> a
  logDensity d = log . density d

  sample :: d -> SampleDomain d -> Domain d

  invSample :: d -> Domain d -> SampleDomain d
  {-# MINIMAL (density | logDensity), sample, invSample #-}
