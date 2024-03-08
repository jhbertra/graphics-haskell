{-# LANGUAGE TemplateHaskell #-}

module Geometry.Shape where

import Control.Lens (makeLenses)
import Data.Maybe (isJust)
import GHC.Generics (Generic, Generic1)
import Geometry.Bounds (Bounds3)
import Geometry.Interaction (Interaction (..), SurfaceInteraction)
import Geometry.Normal (Normal)
import Geometry.Ray (
  IsRay,
  Ray (..),
  RayOrigin (..),
  offsetRayOrigin,
  offsetRayOrigin',
  offsetRayOriginTo',
  spawnRay,
  spawnRay',
  spawnRayTo,
  spawnRayTo',
 )
import Geometry.Spherical (DirectionCone)
import Linear (V2, V3)
import Linear.Affine (Affine ((.-.)), Point)
import Numeric.Interval.IEEE (Interval)
import qualified Numeric.Interval.IEEE as I

class Shape s a | s -> a where
  bounds :: s -> Bounds3 a

  normalBounds :: s -> DirectionCone V3 a

  intersectRay :: (IsRay r) => r a -> a -> s -> Maybe (RayIntersection a)

  rayIntersects :: (IsRay r) => r a -> a -> s -> Bool
  rayIntersects r tMax = isJust . intersectRay r tMax

  surfaceArea :: s -> a

  sampleSurface :: Point V2 a -> s -> Maybe (SurfaceSample a)

  surfacePdf :: SurfaceInteraction a -> s -> a

  sampleSurfaceFrom :: ReferencePoint a -> Point V2 a -> s -> Maybe (SurfaceSample a)

  surfacePdfFrom :: ReferencePoint a -> V3 a -> s -> a

data RayIntersection a = RayIntersection
  { _riInteraction :: SurfaceInteraction a
  , _riTHit :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Show)

data SurfaceSample a = SurfaceSample
  { _ssPoint :: Point V3 (Interval a)
  , _ssNormal :: Normal V3 a
  , _ssParametricCoords :: Point V2 a
  , _ssTime :: a
  , _ssPdf :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Show)

instance RayOrigin SurfaceSample where
  offsetRayOrigin ω SurfaceSample{..} = offsetRayOrigin' _ssPoint _ssNormal ω
  offsetRayOriginTo p SurfaceSample{..} = offsetRayOriginTo' _ssPoint _ssNormal p
  spawnRay ω SurfaceSample{..} = spawnRay' _ssPoint _ssNormal ω _ssTime
  spawnRayTo p SurfaceSample{..} = spawnRayTo' _ssPoint _ssNormal p _ssTime

data ReferencePoint a = ReferencePoint
  { _rpPoint :: Point V3 (Interval a)
  , _rpNormals :: Maybe (Normal V3 a, Normal V3 a)
  , _rpTime :: a
  }
  deriving (Ord, Eq, Generic, Show)

instance RayOrigin ReferencePoint where
  offsetRayOrigin ω ReferencePoint{..} = case _rpNormals of
    Nothing -> I.midpoint <$> _rpPoint
    Just (n, _) -> offsetRayOrigin' _rpPoint n ω
  offsetRayOriginTo p ReferencePoint{..} = case _rpNormals of
    Nothing -> I.midpoint <$> _rpPoint
    Just (n, _) -> offsetRayOriginTo' _rpPoint n p
  spawnRay ω ReferencePoint{..} = case _rpNormals of
    Nothing -> Ray (I.midpoint <$> _rpPoint) ω _rpTime
    Just (n, _) -> spawnRay' _rpPoint n ω _rpTime
  spawnRayTo p rp@ReferencePoint{..} =
    spawnRay (p .-. (I.midpoint <$> _rpPoint)) rp

makeLenses 'RayIntersection
makeLenses 'SurfaceSample

instance Interaction (SurfaceSample a) a where
  interactionPoint = ssPoint
  interactionTime = ssTime
  interactionOutgoingDirection = const pure
