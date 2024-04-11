{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Geometry.Ray where

import Control.Lens (makeLenses, over, _Just)
import GHC.Generics (Generic, Generic1)
import Geometry.Normal (Normal (..))
import Geometry.Parametric (Parametric (..))
import Linear (Epsilon, Metric (..), V3, normalize, (*^), (^*))
import Linear.Affine (Affine (..), Point (..), unP)
import Linear.Affine.Arbitrary ()
import Linear.Arbitrary ()
import Numeric.IEEE (IEEE (..))
import Numeric.Interval.IEEE (Interval)
import qualified Numeric.Interval.IEEE as I
import Test.QuickCheck (Arbitrary, genericShrink)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data Ray a = Ray
  { _o :: Point V3 a
  , _d :: V3 a
  , _rayDifferentials :: Maybe (RayDifferentials a)
  , _time :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

instance (Arbitrary a) => Arbitrary (Ray a) where
  arbitrary = Ray <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Num a) => Parametric (Ray a) V3 a where
  pointAt t Ray{..} = _o .+^ _d ^* t
  {-# INLINE pointAt #-}

data RayDifferentials a = RayDifferentials
  { _rxOrigin :: V3 a
  , _ryOrigin :: V3 a
  , _rxDirection :: V3 a
  , _ryDirection :: V3 a
  }
  deriving (Ord, Eq, Generic, Show, Read, Functor)

instance (Arbitrary a) => Arbitrary (RayDifferentials a) where
  arbitrary =
    RayDifferentials
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

$(makeLenses ''RayDifferentials)
$(makeLenses ''Ray)

scaleDifferentials :: (Num a) => a -> Ray a -> Ray a
scaleDifferentials s = over (rayDifferentials . _Just) \RayDifferentials{..} ->
  RayDifferentials
    { _rxOrigin = _rxOrigin ^* s
    , _ryOrigin = _ryOrigin ^* s
    , _rxDirection = _rxDirection ^* s
    , _ryDirection = _ryDirection ^* s
    }
{-# INLINE scaleDifferentials #-}

class RayOrigin f where
  offsetRayOrigin :: (IEEE a, Epsilon a) => V3 a -> f a -> Point V3 a
  offsetRayOriginTo :: (IEEE a, Epsilon a) => Point V3 a -> f a -> Point V3 a
  spawnRay :: (IEEE a, Epsilon a) => V3 a -> f a -> Ray a
  spawnRayTo :: (IEEE a, Epsilon a) => Point V3 a -> f a -> Ray a

offsetRayOrigin' :: (IEEE a) => Point V3 (Interval a) -> Normal V3 a -> V3 a -> Point V3 a
offsetRayOrigin' p (N n) ω = roundAway <$> P offset <*> p
  where
    δ = dot (abs n) (I.margin <$> unP p)
    offset
      | dot ω n < 0 = negate $ δ *^ n
      | otherwise = δ *^ n
    roundAway offset_i ((+ offset_i) . I.midpoint -> p_i) =
      case compare offset_i 0 of
        GT -> succIEEE p_i
        EQ -> p_i
        LT -> predIEEE p_i

offsetRayOriginTo' :: (IEEE a) => Point V3 (Interval a) -> Normal V3 a -> Point V3 a -> Point V3 a
offsetRayOriginTo' p n = offsetRayOrigin' p n . (.-. (I.midpoint <$> p))

spawnRay' :: (IEEE a) => Point V3 (Interval a) -> Normal V3 a -> V3 a -> a -> Ray a
spawnRay' p n ω = Ray (offsetRayOrigin' p n ω) ω Nothing

spawnRayTo' :: (IEEE a, Epsilon a) => Point V3 (Interval a) -> Normal V3 a -> Point V3 a -> a -> Ray a
spawnRayTo' p n = spawnRay' p n . normalize . (.-. (I.midpoint <$> p))
