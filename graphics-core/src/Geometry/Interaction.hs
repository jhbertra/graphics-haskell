{-# LANGUAGE TemplateHaskell #-}

module Geometry.Interaction where

import Control.Lens (Lens', Traversal', coerced, makeLenses, over, set, traversed, (%~), (&), (^.))
import Data.Function (on)
import Geometry.Normal (Normal (..), faceForward)
import Geometry.Ray (
  RayOrigin (..),
  offsetRayOrigin,
  offsetRayOrigin',
  offsetRayOriginTo',
  spawnRay,
  spawnRay',
  spawnRayTo,
  spawnRayTo',
 )
import Geometry.Transform (ApplyTransform (..))
import Linear
import Linear.Affine
import Numeric.IEEE (IEEE)
import Numeric.Interval.IEEE (Interval)
import qualified Numeric.Interval.IEEE as I

class Interaction i a | i -> a where
  interactionPoint :: Lens' i (Point V3 (Interval a))
  interactionTime :: Lens' i a
  interactionOutgoingDirection :: Traversal' i (V3 a)

data SurfaceInteraction a = SurfaceInteraction
  { _siPoint :: Point V3 (Interval a)
  , _siTime :: a
  , _siOutgoingDirection :: Maybe (V3 a)
  , _siParametricCoords :: Point V2 a
  , _siLocalGeometry :: SurfaceLocalGeometry a
  , _siShadingGeometry :: SurfaceLocalGeometry a
  , _siFaceIndex :: Int
  , _siScreenSpaceDifferentials :: ScreenSpaceDifferentials a
  }
  deriving (Show, Eq, Ord)

instance (IEEE a, Epsilon a) => ApplyTransform SurfaceInteraction a where
  t !!*!! SurfaceInteraction{..} =
    SurfaceInteraction
      { _siPoint = ti !!*!! _siPoint
      , _siOutgoingDirection = normalize . (t !!*!!) <$> _siOutgoingDirection
      , _siLocalGeometry = t !!*!! _siLocalGeometry
      , _siShadingGeometry = t !!*!! _siShadingGeometry
      , ..
      }
    where
      ti = I.singleton <$> t

surfaceInteraction
  :: (Floating a, Epsilon a)
  => Point V3 (Interval a)
  -> a
  -> Maybe (V3 a)
  -> Point V2 a
  -> V3 a
  -> V3 a
  -> Normal V3 a
  -> Normal V3 a
  -> Bool
  -> SurfaceInteraction a
surfaceInteraction _siPoint _siTime _siOutgoingDirection _siParametricCoords dpdu dpdv dndu dndv flipNormal =
  SurfaceInteraction{..}
  where
    _siLocalGeometry = surfaceLocalGeometry dpdu dpdv dndu dndv flipNormal
    _siShadingGeometry = _siLocalGeometry
    _siFaceIndex = 0
    _siScreenSpaceDifferentials = ScreenSpaceDifferentials 0 0 0 0 0 0

instance RayOrigin SurfaceInteraction where
  offsetRayOrigin ω SurfaceInteraction{..} = offsetRayOrigin' _siPoint _surfaceNormal ω
    where
      SurfaceLocalGeometry{..} = _siLocalGeometry
  offsetRayOriginTo p SurfaceInteraction{..} = offsetRayOriginTo' _siPoint _surfaceNormal p
    where
      SurfaceLocalGeometry{..} = _siLocalGeometry
  spawnRay ω SurfaceInteraction{..} = spawnRay' _siPoint _surfaceNormal ω _siTime
    where
      SurfaceLocalGeometry{..} = _siLocalGeometry
  spawnRayTo p SurfaceInteraction{..} = spawnRayTo' _siPoint _surfaceNormal p _siTime
    where
      SurfaceLocalGeometry{..} = _siLocalGeometry

data SurfaceLocalGeometry a = SurfaceLocalGeometry
  { _surfaceNormal :: Normal V3 a
  , _dpdu :: V3 a
  , _dpdv :: V3 a
  , _dndu :: Normal V3 a
  , _dndv :: Normal V3 a
  }
  deriving (Show, Eq, Ord)

instance (Floating a, Epsilon a) => ApplyTransform SurfaceLocalGeometry a where
  t !!*!! SurfaceLocalGeometry{..} =
    SurfaceLocalGeometry
      { _surfaceNormal = t !!*!! _surfaceNormal
      , _dpdu = t !!*!! _dpdu
      , _dpdv = t !!*!! _dpdv
      , _dndu = t !!*!! _dndu
      , _dndv = t !!*!! _dndv
      }

surfaceLocalGeometry
  :: (Floating a, Epsilon a)
  => V3 a
  -> V3 a
  -> Normal V3 a
  -> Normal V3 a
  -> Bool
  -> SurfaceLocalGeometry a
surfaceLocalGeometry _dpdu _dpdv _dndu _dndv flipNormal = SurfaceLocalGeometry{..}
  where
    _surfaceNormal
      | flipNormal = N $ normalize $ on cross unN _dndv _dndu
      | otherwise = N $ normalize $ on cross unN _dndu _dndv

data ScreenSpaceDifferentials a = ScreenSpaceDifferentials
  { _dpdx :: V3 a
  , _dpdy :: V3 a
  , _dudx :: a
  , _dudy :: a
  , _dvdx :: a
  , _dvdy :: a
  }
  deriving (Show, Eq, Ord)

makeLenses 'SurfaceInteraction
makeLenses 'SurfaceLocalGeometry
makeLenses 'ScreenSpaceDifferentials

setShadingGeometry
  :: (Num a, Ord a)
  => SurfaceLocalGeometry a
  -> Bool
  -> SurfaceInteraction a
  -> SurfaceInteraction a
setShadingGeometry shading useShadingOrientation
  | useShadingOrientation =
      over (siLocalGeometry . surfaceNormal) (faceForward (shading ^. surfaceNormal . coerced))
        . set siShadingGeometry shading
  | otherwise = \SurfaceInteraction{..} ->
      SurfaceInteraction
        { _siShadingGeometry =
            shading & surfaceNormal %~ faceForward (_siLocalGeometry ^. surfaceNormal . coerced)
        , ..
        }

instance Interaction (SurfaceInteraction a) a where
  interactionPoint = siPoint
  interactionTime = siTime
  interactionOutgoingDirection = siOutgoingDirection . traversed
