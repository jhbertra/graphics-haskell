module Geometry.Shape.Disk (
  Disk,
  _diskFlipNormals,
  _diskFromRender,
  _diskHeight,
  _diskInnerRadius,
  _diskPhiMax,
  _diskRadius,
  _diskToRender,
  _diskTransformSwapsHandedness,
  disk,
  diskFlipNormals,
  diskHeight,
  diskInnerRadius,
  diskPhiMax,
  diskRadius,
) where

import Control.Lens (Lens', lens, view)
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import GHC.Show (showSpace)
import qualified Geometry.Bounds as Bounds
import Geometry.Interaction (SurfaceInteraction, SurfaceLocalGeometry (..), surfaceInteraction)
import Geometry.Normal (Normal (..))
import Geometry.Ray (IsRay (..), Ray (..), RayOrigin (..))
import Geometry.Shape
import Geometry.Shape.Sphere (QuadricIntersection (..), shrinkTowards)
import Geometry.Spherical (atan2', toDirectionCone)
import Geometry.Transform
import Linear
import Linear.Affine
import Numeric.IEEE
import qualified Numeric.Interval.IEEE as I
import Statistics.Distribution2.UniformDisk (sampleUniformDisk, uniformPartialDiskDistributionE)
import System.Random (Random)
import Test.QuickCheck
import Text.Read (Lexeme (..), Read (..), lexP, parens, prec)

data Disk a = Disk
  { _diskRadius :: a
  , _diskInnerRadius :: a
  , _diskHeight :: a
  , _diskPhiMax :: a
  , _diskFlipNormals :: Bool
  , _diskFromRender :: Transform a
  , _diskToRender :: Transform a
  , _diskTransformSwapsHandedness :: Bool
  }

disk
  :: (Floating a, Ord a)
  => Transform a
  -> Transform a
  -> Bool
  -> a
  -> a
  -> a
  -> a
  -> Disk a
disk _diskToRender _diskFromRender _diskFlipNormals (abs -> radius) (abs -> innerRadius) _diskHeight phiMax =
  Disk
    { _diskRadius = radius
    , _diskInnerRadius = min radius innerRadius
    , _diskHeight
    , _diskPhiMax = clamp (0, 2 * pi) phiMax
    , _diskFlipNormals
    , _diskFromRender
    , _diskToRender
    , _diskTransformSwapsHandedness = swapsHandedness _diskToRender
    }

diskRadius :: (Ord a) => Lens' (Disk a) a
diskRadius = lens _diskRadius \Disk{..} radius ->
  Disk
    { _diskRadius = radius
    , _diskInnerRadius = min radius _diskInnerRadius
    , ..
    }

diskInnerRadius :: (Ord a) => Lens' (Disk a) a
diskInnerRadius = lens _diskRadius \Disk{..} innerRadius ->
  Disk
    { _diskInnerRadius = min _diskRadius innerRadius
    , ..
    }

diskHeight :: Lens' (Disk a) a
diskHeight = lens _diskHeight \c _diskHeight -> c{_diskHeight}

diskPhiMax :: (Floating a, Ord a) => Lens' (Disk a) a
diskPhiMax = lens _diskPhiMax \s phiMax ->
  s{_diskPhiMax = clamp (0, 2 * pi) phiMax}

diskFlipNormals :: Lens' (Disk a) Bool
diskFlipNormals = lens _diskFlipNormals \s _diskFlipNormals -> s{_diskFlipNormals}

instance (Eq a) => Eq (Disk a) where
  a == b =
    on (==) _diskRadius a b
      && on (==) _diskHeight a b
      && on (==) _diskPhiMax a b
      && on (==) _diskFlipNormals a b
  a /= b =
    on (/=) _diskRadius a b
      || on (/=) _diskHeight a b
      || on (/=) _diskPhiMax a b
      || on (/=) _diskFlipNormals a b

instance (Ord a) => Ord (Disk a) where
  compare a b =
    on compare _diskRadius a b
      <> on compare _diskHeight a b
      <> on compare _diskPhiMax a b
      <> on compare _diskFlipNormals a b

instance (Show a) => Show (Disk a) where
  showsPrec p Disk{..} =
    showParen
      (p > 10)
      ( showString "disk"
          . showSpace
          . showsPrec 11 _diskToRender
          . showSpace
          . showsPrec 11 _diskFromRender
          . showSpace
          . showsPrec 11 _diskFlipNormals
          . showSpace
          . showsPrec 11 _diskRadius
          . showSpace
          . showsPrec 11 _diskInnerRadius
          . showSpace
          . showsPrec 11 _diskHeight
          . showSpace
          . showsPrec 11 _diskPhiMax
      )

instance (Floating a, Read a, Ord a) => Read (Disk a) where
  readPrec = parens $ prec 10 do
    Ident "disk" <- lexP
    disk <$> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec

instance (Arbitrary a, IEEE a, Random a, Epsilon a) => Arbitrary (Disk a) where
  arbitrary = do
    toRender <- arbitrary `suchThat` (not . nearZero . detTransform)
    flipNormals <- arbitrary
    radius <- abs <$> arbitrary
    innerRadius <- choose (0, radius)
    height <- arbitrary
    ϕMax <- choose (0, 2 * pi)
    pure $ disk toRender (invTransform toRender) flipNormals radius innerRadius height ϕMax
  shrink Disk{..} =
    [ disk
      toRender'
      (invTransform toRender')
      _diskFlipNormals
      _diskRadius
      _diskInnerRadius
      _diskHeight
      _diskPhiMax
    | toRender' <- shrink _diskToRender
    ]
      ++ [ disk
          _diskToRender
          (invTransform _diskToRender)
          _diskFlipNormals'
          _diskRadius
          _diskInnerRadius
          _diskHeight
          _diskPhiMax
         | _diskFlipNormals' <- shrink _diskFlipNormals
         ]
      ++ [ disk
          _diskToRender
          (invTransform _diskToRender)
          _diskFlipNormals
          _diskRadius'
          (min _diskRadius' _diskInnerRadius)
          _diskHeight
          _diskPhiMax
         | _diskRadius' <- shrinkTowards 1 _diskRadius
         ]
      ++ [ disk
          _diskToRender
          (invTransform _diskToRender)
          _diskFlipNormals
          _diskRadius
          _diskInnerRadius'
          _diskHeight
          _diskPhiMax
         | _diskInnerRadius' <- shrinkTowards 0 _diskRadius
         ]
      ++ [ disk
          _diskToRender
          (invTransform _diskToRender)
          _diskFlipNormals
          _diskRadius
          _diskInnerRadius
          _diskHeight'
          _diskPhiMax
         | _diskHeight' <- shrinkTowards 1 _diskHeight
         ]
      ++ [ disk
          _diskToRender
          (invTransform _diskToRender)
          _diskFlipNormals
          _diskRadius
          _diskInnerRadius
          _diskHeight
          _diskPhiMax'
         | _diskPhiMax' <- shrinkTowards (2 * pi) _diskPhiMax
         ]

instance (IEEE a, Epsilon a, Bounded a) => Shape (Disk a) a where
  bounds Disk{..} =
    _diskToRender
      !!*!! Bounds.Bounds
        (P (V3 (-_diskRadius) (-_diskRadius) _diskHeight))
        (P (V3 _diskRadius _diskRadius _diskHeight))

  normalBounds Disk{..} =
    toDirectionCone $
      _diskToRender
        !!*!! N
          ( V3
              0
              0
              if _diskFlipNormals `xor'` _diskTransformSwapsHandedness
                then -1
                else 1
          )

  surfaceArea Disk{..} =
    _diskPhiMax * 0.5 * (_diskRadius - _diskInnerRadius) * (_diskRadius + _diskInnerRadius)

  intersectRay (view ray -> r) tMax s = do
    isect@QuadricIntersection{..} <- intersectRayQuadric r tMax s
    pure $ RayIntersection (interactionFromQuadric r isect s) _qiTHit

  sampleSurface u s@Disk{..} = do
    guard $ detTransform _diskToRender /= 0
    P (V2 xUnit yUnit) <-
      if _diskInnerRadius == 0 && _diskPhiMax == 2 * pi
        then pure $ sampleUniformDiskConcentric u
        else do
          d <- uniformPartialDiskDistributionE (_diskInnerRadius / _diskRadius) _diskPhiMax
          pure $ sampleUniformDisk d u
    let xObj = xUnit * _diskRadius
    let yObj = yUnit * _diskRadius
    let pObj = P $ V3 xObj yObj _diskHeight
    let _ssPoint = (I.singleton <$> _diskToRender) !!*!! (I.singleton <$> pObj)
    let _ssNormal
          | xor' _diskFlipNormals _diskTransformSwapsHandedness = negate $ _diskToRender !!*!! N (V3 0 0 1)
          | otherwise = _diskToRender !!*!! N (V3 0 0 1)
    let ϕ = atan2' yObj xObj
    let radiusSample = sqrt $ xObj * xObj + yObj * yObj
    let _ssParametricCoords =
          P $
            V2
              (ϕ / _diskPhiMax)
              ((_diskRadius - radiusSample) / (_diskRadius - _diskInnerRadius))
    pure
      SurfaceSample
        { _ssPoint
        , _ssNormal
        , _ssParametricCoords
        , _ssPdf = recip $ surfaceArea s
        , _ssTime = 0
        }

  surfacePdf _ = recip . surfaceArea

  sampleSurfaceFrom ReferencePoint{..} u c = do
    SurfaceSample{..} <- sampleSurface u c
    -- vector from observer point to sample point
    let ωi = I.midpoint <$> _ssPoint .-. _rpPoint
    -- Squared distance of ωi
    let r = quadrance ωi
    -- If the distance is zero, the sample point is not valid.
    guard $ r > 0
    -- Transform the Area-based disk PDF to a solid-angle-based PDF by
    -- dividing by a factor of |cos θ| / |ωi|^2, where θ is the angle between
    -- ωi and the surface normal n.
    let cosθ = case _rpNormals of
          Nothing -> 1 -- We are sampling from a point without a normal (i.e. a medium), choose 1 for the cosine factor.
          Just (N n, _) -> abs $ dot n $ negate $ normalize ωi
    -- If the sampled point lies tangent to the surface, the sample is
    -- invalid.
    guard $ cosθ > 0
    pure
      SurfaceSample
        { _ssTime = _rpTime
        , _ssPdf = _ssPdf * r / cosθ
        , ..
        }

  surfacePdfFrom rp@ReferencePoint{..} ωi c = fromMaybe 0 do
    let r = spawnRay ωi rp
    QuadricIntersection{..} <- intersectRayQuadric r infinity c
    -- Transform the Area-based disk PDF to a solid-angle-based PDF by
    -- dividing by a factor of |cos θ| / |r|^2, where θ is the angle between
    -- r and the surface normal n, and r is the vector from the observer
    -- point to the sample point.
    let cosθ = case _rpNormals of
          Nothing -> 1 -- We are sampling from a point without a normal (i.e. a medium), choose 1 for the cosine factor.
          Just (N n, _) -> abs $ dot n $ -ωi
    -- If the sampled point lies tangent to the surface, the sample is
    -- invalid.
    guard $ cosθ > 0
    let squaredDistanceToPoint = qdA (I.midpoint <$> _rpPoint) _qiPObj
    pure $ squaredDistanceToPoint / (surfaceArea c * cosθ)

sampleUniformDiskConcentric :: (Floating a, Ord a) => Point V2 a -> Point V2 a
sampleUniformDiskConcentric 0 = 0
sampleUniformDiskConcentric (P (V2 ξ0 ξ1)) = P $ r *^ V2 (cos θ) (sin θ)
  where
    x = ξ0 * 2 - 1
    y = ξ1 * 2 - 1
    (r, θ)
      | abs x > abs y = (x, pi * 0.25 * (y / x))
      | otherwise = (y, pi * 0.5 - pi * 0.25 * (x / y))

intersectRayQuadric :: (IEEE a) => Ray a -> a -> Disk a -> Maybe (QuadricIntersection a)
intersectRayQuadric Ray{..} tMax Disk{..} = do
  let d@(V3 _ _ dz) = _diskFromRender !!*!! _d
  guard $ dz /= 0
  let o@(P (V3 _ _ oz)) = _diskFromRender !!*!! _o
  let tHit = (_diskHeight - oz) / dz
  guard $ tHit > 0 && tHit <= tMax
  let pHit@(P (V3 x y _)) = o .+^ tHit *^ d
  let rHit² = x * x + y * y
  let r² = _diskRadius * _diskRadius
  let ri² = _diskInnerRadius * _diskInnerRadius
  let ϕ = atan2' y x
  guard $ rHit² <= r² && rHit² >= ri² && ϕ <= _diskPhiMax
  pure $ QuadricIntersection tHit pHit ϕ

interactionFromQuadric :: (Epsilon a, IEEE a) => Ray a -> QuadricIntersection a -> Disk a -> SurfaceInteraction a
interactionFromQuadric Ray{..} QuadricIntersection{..} Disk{..} =
  _diskToRender
    !!*!! surfaceInteraction
      (I.singleton <$> _qiPObj)
      _time
      (Just $ _diskFromRender !!*!! (-_d))
      (P $ V2 u v)
      localGeometry
  where
    P (V3 xHit yHit _) = _qiPObj
    u = _qiPhi / _diskPhiMax
    rHit = sqrt $ xHit * xHit + yHit + yHit
    v = (_diskRadius - rHit) / (_diskRadius - _diskInnerRadius)
    _dpdu = V3 (-_diskPhiMax * yHit) (_diskPhiMax * xHit) 0
    _dpdv = V3 xHit yHit 0 ^* (_diskInnerRadius - _diskRadius) ^/ rHit
    _dndu = 0
    _dndv = 0
    _surfaceNormal
      | _diskTransformSwapsHandedness `xor'` _diskFlipNormals = N $ V3 0 0 1
      | otherwise = N $ V3 0 0 (-1)
    localGeometry = SurfaceLocalGeometry{..}

xor' :: Bool -> Bool -> Bool
xor' False False = False
xor' False True = True
xor' True False = True
xor' True True = False
