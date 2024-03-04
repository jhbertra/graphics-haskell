{-# LANGUAGE TemplateHaskell #-}

module Geometry.Shape where

import Control.Lens (makeLenses)
import Data.Maybe (isJust)
import GHC.Generics (Generic, Generic1)
import Geometry.Bounds (Bounds3)
import Geometry.Interaction (SurfaceInteraction)
import Geometry.Ray (IsRay)
import Geometry.Spherical (DirectionCone)
import Linear (V2, V3)
import Linear.Affine (Point)
import Numeric.Interval (Interval)

class Shape s a | s -> a where
  bounds :: s -> Bounds3 a

  normalBounds :: s -> DirectionCone V3 a

  intersectRay :: (IsRay r) => r a -> a -> s -> Maybe (RayIntersection a)

  rayIntersects :: (IsRay r) => r a -> a -> s -> Bool
  rayIntersects r tMax = isJust . intersectRay r tMax

  surfaceArea :: s -> a

  sampleSurface :: Point V2 a -> Maybe (SurfaceSample a)

  surfacePdf :: SurfaceInteraction a -> a

  sampleSurfaceFrom :: ReferencePoint a -> Point V2 a -> Maybe (SurfaceSample a)

  surfacePdfFrom :: ReferencePoint a -> SurfaceInteraction a -> a

data RayIntersection a = RayIntersection
  { _riInteraction :: SurfaceInteraction a
  , _riTHit :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Show)

data SurfaceSample a = SurfaceSample
  { _ssInteraction :: SurfaceInteraction a
  , _ssPdf :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Show)

data ReferencePoint a = ReferencePoint
  { _rpPoint :: Point V3 (Interval a)
  , _rpNormals :: Maybe (V3 a, V3 a)
  , _rpTime :: a
  }
  deriving (Ord, Eq, Generic, Show)

makeLenses 'RayIntersection
makeLenses 'SurfaceSample
