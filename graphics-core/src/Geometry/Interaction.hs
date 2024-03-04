{-# LANGUAGE TemplateHaskell #-}

module Geometry.Interaction where

import Control.Lens (Lens', makeLenses, over, set, (%~), (&), (^.))
import Linear
import Linear.Affine
import Numeric.Interval (Interval)

class Interaction i a | i -> a where
  interactionPoint :: Lens' i (Point V3 (Interval a))
  interactionTime :: Lens' i a
  interactionOutgoingDirection :: Lens' i (Maybe (V3 a))

data SurfaceInteraction a = SurfaceInteraction
  { _siPoint :: Point V3 (Interval a)
  , _siTime :: a
  , _siOutgoingDirection :: Maybe (V3 a)
  , _siParametricCoords :: V2 a
  , _siLocalGeometry :: SurfaceLocalGeometry a
  , _siShadingGeometry :: SurfaceLocalGeometry a
  , _siFaceIndex :: Int
  , _siScreenSpaceDifferentials :: ScreenSpaceDifferentials a
  }
  deriving (Show, Eq, Ord)

surfaceInteraction
  :: (Floating a, Epsilon a)
  => Point V3 (Interval a)
  -> a
  -> Maybe (V3 a)
  -> V2 a
  -> V3 a
  -> V3 a
  -> V3 a
  -> V3 a
  -> Bool
  -> SurfaceInteraction a
surfaceInteraction _siPoint _siTime _siOutgoingDirection _siParametricCoords dpdu dpdv dndu dndv flipNormal =
  SurfaceInteraction{..}
  where
    _siLocalGeometry = surfaceLocalGeometry dpdu dpdv dndu dndv flipNormal
    _siShadingGeometry = _siLocalGeometry
    _siFaceIndex = 0
    _siScreenSpaceDifferentials = ScreenSpaceDifferentials 0 0 0 0 0 0

data SurfaceLocalGeometry a = SurfaceLocalGeometry
  { _surfaceNormal :: V3 a
  , _dpdu :: V3 a
  , _dpdv :: V3 a
  , _dndu :: V3 a
  , _dndv :: V3 a
  }
  deriving (Show, Eq, Ord)

surfaceLocalGeometry
  :: (Floating a, Epsilon a)
  => V3 a
  -> V3 a
  -> V3 a
  -> V3 a
  -> Bool
  -> SurfaceLocalGeometry a
surfaceLocalGeometry _dpdu _dpdv _dndu _dndv flipNormal = SurfaceLocalGeometry{..}
  where
    _surfaceNormal
      | flipNormal = normalize $ cross _dndv _dndu
      | otherwise = normalize $ cross _dndu _dndv

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
      over (siLocalGeometry . surfaceNormal) (faceForward (shading ^. surfaceNormal)) . set siShadingGeometry shading
  | otherwise = \SurfaceInteraction{..} ->
      SurfaceInteraction
        { _siShadingGeometry = shading & surfaceNormal %~ faceForward (_siLocalGeometry ^. surfaceNormal)
        , ..
        }

faceForward :: (Num a, Metric f, Ord a, Num (f a)) => f a -> f a -> f a
faceForward a b
  | dot a b < 0 = -b
  | otherwise = b

instance Interaction (SurfaceInteraction a) a where
  interactionPoint = siPoint
  interactionTime = siTime
  interactionOutgoingDirection = siOutgoingDirection
