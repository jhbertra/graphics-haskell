module Geometry.Shape.Cylinder (
  Cylinder,
  _cylinderFromRender,
  _cylinderTransformSwapsHandedness,
  _cylinderFlipNormals,
  _cylinderPhiMax,
  _cylinderRadius,
  _cylinderToRender,
  _cylinderHeight,
  cylinder,
  cylinderFlipNormals,
  cylinderPhiMax,
  cylinderRadius,
  cylinderHeight,
) where

import Control.Applicative (Alternative (..))
import Control.Lens (Lens', lens, view)
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import GHC.Show (showSpace)
import qualified Geometry.Bounds as Bounds
import Geometry.Interaction (SurfaceInteraction, surfaceInteraction, surfaceLocalGeometry)
import Geometry.Normal (Normal (..))
import Geometry.Ray (IsRay (..), Ray (..), RayOrigin (..))
import Geometry.Shape
import Geometry.Shape.Sphere (QuadricIntersection (..), shrinkTowards)
import Geometry.Spherical (allDirections, atan2')
import Geometry.Transform
import Linear
import Linear.Affine
import Numeric.IEEE
import qualified Numeric.Interval.IEEE as I
import System.Random (Random)
import Test.QuickCheck
import Text.Read (Lexeme (..), Read (..), lexP, parens, prec)

data Cylinder a = Cylinder
  { _cylinderRadius :: a
  , _cylinderHeight :: a
  , _cylinderPhiMax :: a
  , _cylinderFlipNormals :: Bool
  , _cylinderFromRender :: Transform a
  , _cylinderToRender :: Transform a
  , _cylinderTransformSwapsHandedness :: Bool
  }

cylinder
  :: (Floating a, Ord a)
  => Transform a
  -> Transform a
  -> Bool
  -> a
  -> a
  -> a
  -> Cylinder a
cylinder _cylinderToRender _cylinderFromRender _cylinderFlipNormals (abs -> radius) _cylinderHeight phiMax =
  Cylinder
    { _cylinderRadius = radius
    , _cylinderHeight
    , _cylinderPhiMax = clamp (0, 2 * pi) phiMax
    , _cylinderFlipNormals
    , _cylinderFromRender
    , _cylinderToRender
    , _cylinderTransformSwapsHandedness = swapsHandedness _cylinderToRender
    }

cylinderRadius :: Lens' (Cylinder a) a
cylinderRadius = lens _cylinderRadius \c _cylinderRadius -> c{_cylinderRadius}

cylinderHeight :: Lens' (Cylinder a) a
cylinderHeight = lens _cylinderHeight \c _cylinderHeight -> c{_cylinderHeight}

cylinderPhiMax :: (Floating a, Ord a) => Lens' (Cylinder a) a
cylinderPhiMax = lens _cylinderPhiMax \s phiMax ->
  s{_cylinderPhiMax = clamp (0, 2 * pi) phiMax}

cylinderFlipNormals :: Lens' (Cylinder a) Bool
cylinderFlipNormals = lens _cylinderFlipNormals \s _cylinderFlipNormals -> s{_cylinderFlipNormals}

instance (Eq a) => Eq (Cylinder a) where
  a == b =
    on (==) _cylinderRadius a b
      && on (==) _cylinderHeight a b
      && on (==) _cylinderPhiMax a b
      && on (==) _cylinderFlipNormals a b
  a /= b =
    on (/=) _cylinderRadius a b
      || on (/=) _cylinderHeight a b
      || on (/=) _cylinderPhiMax a b
      || on (/=) _cylinderFlipNormals a b

instance (Ord a) => Ord (Cylinder a) where
  compare a b =
    on compare _cylinderRadius a b
      <> on compare _cylinderHeight a b
      <> on compare _cylinderPhiMax a b
      <> on compare _cylinderFlipNormals a b

instance (Show a) => Show (Cylinder a) where
  showsPrec p Cylinder{..} =
    showParen
      (p > 10)
      ( showString "cylinder"
          . showSpace
          . showsPrec 11 _cylinderToRender
          . showSpace
          . showsPrec 11 _cylinderFromRender
          . showSpace
          . showsPrec 11 _cylinderFlipNormals
          . showSpace
          . showsPrec 11 _cylinderRadius
          . showSpace
          . showsPrec 11 _cylinderHeight
          . showSpace
          . showsPrec 11 _cylinderPhiMax
      )

instance (Floating a, Read a, Ord a) => Read (Cylinder a) where
  readPrec = parens $ prec 10 do
    Ident "cylinder" <- lexP
    cylinder <$> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec

instance (Arbitrary a, IEEE a, Random a, Epsilon a) => Arbitrary (Cylinder a) where
  arbitrary = do
    toRender <- arbitrary `suchThat` (not . nearZero . detTransform)
    flipNormals <- arbitrary
    radius <- abs <$> arbitrary
    height <- arbitrary
    ϕMax <- choose (0, 2 * pi)
    pure $ cylinder toRender (invTransform toRender) flipNormals radius height ϕMax
  shrink Cylinder{..} =
    [ cylinder
      toRender'
      (invTransform toRender')
      _cylinderFlipNormals
      _cylinderRadius
      _cylinderHeight
      _cylinderPhiMax
    | toRender' <- shrink _cylinderToRender
    ]
      ++ [ cylinder
          _cylinderToRender
          (invTransform _cylinderToRender)
          _cylinderFlipNormals'
          _cylinderRadius
          _cylinderHeight
          _cylinderPhiMax
         | _cylinderFlipNormals' <- shrink _cylinderFlipNormals
         ]
      ++ [ cylinder
          _cylinderToRender
          (invTransform _cylinderToRender)
          _cylinderFlipNormals
          _cylinderRadius'
          _cylinderHeight
          _cylinderPhiMax
         | _cylinderRadius' <- shrinkTowards 1 _cylinderRadius
         ]
      ++ [ cylinder
          _cylinderToRender
          (invTransform _cylinderToRender)
          _cylinderFlipNormals
          _cylinderRadius
          _cylinderHeight'
          _cylinderPhiMax
         | _cylinderHeight' <- shrinkTowards 1 _cylinderHeight
         ]
      ++ [ cylinder
          _cylinderToRender
          (invTransform _cylinderToRender)
          _cylinderFlipNormals
          _cylinderRadius
          _cylinderHeight
          _cylinderPhiMax'
         | _cylinderPhiMax' <- shrinkTowards (2 * pi) _cylinderPhiMax
         ]

instance (IEEE a, Epsilon a, Bounded a) => Shape (Cylinder a) a where
  bounds Cylinder{..} =
    _cylinderToRender
      !!*!! Bounds.Bounds
        (P (V3 (-_cylinderRadius) (-_cylinderRadius) (min _cylinderHeight 0)))
        (P (V3 _cylinderRadius _cylinderRadius (max _cylinderHeight 0)))

  normalBounds _ = allDirections

  surfaceArea Cylinder{..} = _cylinderHeight * _cylinderRadius * _cylinderPhiMax

  intersectRay (view ray -> r) tMax s = do
    isect@QuadricIntersection{..} <- intersectRayQuadric r tMax s
    pure $ RayIntersection (interactionFromQuadric r isect s) _qiTHit

  sampleSurface (P (V2 ξ0 ξ1)) c@Cylinder{..} = do
    guard $ detTransform _cylinderToRender /= 0
    let z = ξ0 * _cylinderHeight
    let ϕ = ξ1 * _cylinderPhiMax
    let x' = _cylinderRadius * cos ϕ
    let y' = _cylinderRadius * sin ϕ
    let radius' = sqrt $ (x' * x') + (y' * y')
    let correction = _cylinderRadius / radius'
    let x = x' * correction
    let y = y' * correction
    let _ssPoint =
          (I.singleton <$> _cylinderToRender)
            !!*!! (I.fromMidpointAndMargin <$> P (V3 x y z) <*> (I.gamma 3 *^ abs (P $ V3 x y 0)))
    let _ssNormal
          | xor' _cylinderFlipNormals _cylinderTransformSwapsHandedness = negate $ _cylinderToRender !!*!! N (V3 x y z)
          | otherwise = _cylinderToRender !!*!! N (V3 x y z)
    let _ssParametricCoords =
          P $
            V2
              (ϕ / _cylinderPhiMax)
              (z / _cylinderHeight)
    pure
      SurfaceSample
        { _ssPoint
        , _ssNormal
        , _ssParametricCoords
        , _ssPdf = recip $ surfaceArea c
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
    -- Transform the Area-based cylinder PDF to a solid-angle-based PDF by
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
    -- Transform the Area-based cylinder PDF to a solid-angle-based PDF by
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

intersectRayQuadric :: (IEEE a) => Ray a -> a -> Cylinder a -> Maybe (QuadricIntersection a)
intersectRayQuadric Ray{..} tMax Cylinder{..} = do
  guard $ discriminant >= 0
  let rootDiscriminant = sqrt discriminant
      q
        | I.midpoint b < 0 = -0.5 * (b - rootDiscriminant)
        | otherwise = -0.5 * (b + rootDiscriminant)
      t0 = q / a
      t1 = c / q
      (tNear, tFar)
        | t0 > t1 = (t1, t0)
        | otherwise = (t0, t1)
      validateHit t = do
        guard (I.sup t <= tMax && I.inf t > 0)
        let P (V3 xHit' yHit' zHit) = I.midpoint <$> P (o .+^ t *^ d)
            hitRadius = sqrt $ (xHit' * xHit') + (yHit' * yHit')
            correction = _cylinderRadius / hitRadius
            xHit = xHit' * correction
            yHit = yHit' * correction
            pHit = P $ V3 xHit yHit zHit
            phi = atan2' yHit xHit
        guard $
          zHit >= min 0 _cylinderHeight
            && zHit <= max 0 _cylinderHeight
            && phi <= _cylinderPhiMax
        pure $ QuadricIntersection (I.midpoint t) pHit phi
  validateHit tNear <|> validateHit tFar
  where
    fromRender = I.singleton <$> _cylinderFromRender
    P o@(V3 ox oy _) = fromRender !!*!! (I.singleton <$> _o)
    d@(V3 dx dy _) = fromRender !!*!! (I.singleton <$> _d)
    a = I.square dx + I.square dy
    b = I.mulPow2 2 $ dx * ox + dy * oy
    ri = I.singleton _cylinderRadius
    c = I.square ox + I.square oy - I.square ri
    f = I.mulPow2 0.5 $ b / a
    vx = ox - f * dx
    vy = oy - f * dy
    len = sqrt $ I.square vx + I.square vy
    discriminant = I.mulPow2 4 $ a * (ri + len) * (ri - len)

interactionFromQuadric :: (Epsilon a, IEEE a) => Ray a -> QuadricIntersection a -> Cylinder a -> SurfaceInteraction a
interactionFromQuadric Ray{..} QuadricIntersection{..} Cylinder{..} =
  _cylinderToRender
    !!*!! surfaceInteraction
      siP
      _time
      (Just $ _cylinderFromRender !!*!! (-_d))
      (P $ V2 u v)
      ( surfaceLocalGeometry
          dpdu
          dpdv
          dndu
          dndv
          (_cylinderTransformSwapsHandedness `xor'` _cylinderFlipNormals)
      )
  where
    P (V3 xHit yHit zHit) = _qiPObj
    siP = I.fromMidpointAndMargin <$> _qiPObj <*> (I.gamma 3 *^ abs (P $ V3 xHit yHit 0))
    u = _qiPhi / _cylinderPhiMax
    v = zHit / _cylinderHeight
    dpdu = V3 (-_cylinderPhiMax * yHit) (_cylinderPhiMax * xHit) 0
    dpdv = V3 0 0 _cylinderHeight
    d²pdu² = -_cylinderPhiMax * _cylinderPhiMax *^ V3 xHit yHit 0
    d²pdvdv = 0
    d²pdv² = 0
    _E = quadrance dpdu
    _F = dot dpdu dpdv
    _G = quadrance dpdv
    n = normalize $ cross dpdu dpdv
    e = dot n d²pdu²
    f = dot n d²pdvdv
    g = dot n d²pdv²
    _EGF2 = _E * _G - _F * _F
    invEGF2
      | _EGF2 == 0 = 0
      | otherwise = recip _EGF2
    dndu =
      N $
        (f * _F - e * _G) * invEGF2 *^ dpdu
          + (e * _F - f * _E) * invEGF2 *^ dpdv
    dndv =
      N $
        (g * _F - f * _G) * invEGF2 *^ dpdu
          + (f * _F - g * _E) * invEGF2 *^ dpdv

xor' :: Bool -> Bool -> Bool
xor' False False = False
xor' False True = True
xor' True False = True
xor' True True = False
