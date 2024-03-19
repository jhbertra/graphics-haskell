module Geometry.Shape.Sphere (
  QuadricIntersection (..),
  Sphere,
  _sphereFromRender,
  _sphereTransformSwapsHandedness,
  _sphereFlipNormals,
  _spherePhiMax,
  _sphereRadius,
  _sphereThetaMax,
  _sphereThetaMin,
  _sphereToRender,
  _sphereZMax,
  _sphereZMin,
  sphere,
  sphereFlipNormals,
  spherePhiMax,
  sphereRadius,
  sphereThetaMax,
  sphereThetaMin,
  sphereZMax,
  sphereZMin,
  sphereDistribution,
  shrinkTowards,
) where

import Control.Applicative ((<|>))
import Control.Lens (Lens', lens, view, (^.))
import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (clamp)
import GHC.Show (showSpace)
import qualified Geometry.Bounds as Bounds
import Geometry.Interaction (SurfaceInteraction (..), surfaceInteraction, surfaceLocalGeometry)
import Geometry.Normal (Normal (..))
import Geometry.Ray (IsRay (..), Ray (..), RayOrigin (..))
import Geometry.Shape (RayIntersection (..), ReferencePoint (..), Shape (..), SurfaceSample (..))
import Geometry.Spherical (SphericalCoordinates (..), allDirections, atan2', safeAcos, safeSqrt, sphericalDirection)
import Geometry.Transform (
  ApplyTransform (..),
  Transform,
  detTransform,
  frameFromZ,
  fromLocal,
  invTransform,
  swapsHandedness,
 )
import Linear
import Linear.Affine
import Numeric.IEEE (IEEE (..))
import Numeric.Interval.IEEE (mulPow2)
import qualified Numeric.Interval.IEEE as I
import Statistics.Distribution2 (sampleContinuous2_XY, sampleContinuous2_YX)
import Statistics.Distribution2.UniformCone
import Statistics.Distribution2.UniformSphere (
  UniformSphereDistribution,
  uniformPartialSphereDistributionE,
 )
import System.Random (Random)
import Test.QuickCheck
import Text.Read (Lexeme (..), Read (..), lexP, parens, prec)

data Sphere a = Sphere
  { _sphereRadius :: a
  , _sphereZMin :: a
  , _sphereZMax :: a
  , _sphereThetaMin :: a
  , _sphereThetaMax :: a
  , _spherePhiMax :: a
  , _sphereFlipNormals :: Bool
  , _sphereFromRender :: Transform a
  , _sphereToRender :: Transform a
  , _sphereTransformSwapsHandedness :: Bool
  }

instance (Eq a) => Eq (Sphere a) where
  a == b =
    on (==) _sphereRadius a b
      && on (==) _sphereZMin a b
      && on (==) _sphereZMax a b
      && on (==) _spherePhiMax a b
      && on (==) _sphereFlipNormals a b
  a /= b =
    on (/=) _sphereRadius a b
      || on (/=) _sphereZMin a b
      || on (/=) _sphereZMax a b
      || on (/=) _spherePhiMax a b
      || on (/=) _sphereFlipNormals a b

instance (Ord a) => Ord (Sphere a) where
  compare a b =
    on compare _sphereRadius a b
      <> on compare _sphereZMin a b
      <> on compare _sphereZMax a b
      <> on compare _spherePhiMax a b
      <> on compare _sphereFlipNormals a b

instance (Show a) => Show (Sphere a) where
  showsPrec p Sphere{..} =
    showParen
      (p > 10)
      ( showString "sphere"
          . showSpace
          . showsPrec 11 _sphereToRender
          . showSpace
          . showsPrec 11 _sphereFromRender
          . showSpace
          . showsPrec 11 _sphereFlipNormals
          . showSpace
          . showsPrec 11 _sphereRadius
          . showSpace
          . showsPrec 11 _sphereZMin
          . showSpace
          . showsPrec 11 _sphereZMax
          . showSpace
          . showsPrec 11 _spherePhiMax
      )

instance (Floating a, Read a, Ord a) => Read (Sphere a) where
  readPrec = parens $ prec 10 do
    Ident "sphere" <- lexP
    sphere <$> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec

instance (IEEE a, Epsilon a, Bounded a) => Shape (Sphere a) a where
  bounds Sphere{..} =
    _sphereToRender
      !!*!! Bounds.Bounds
        (P (V3 (-_sphereRadius) (-_sphereRadius) _sphereZMin))
        (P (V3 _sphereRadius _sphereRadius _sphereZMax))

  normalBounds _ = allDirections

  intersectRay (view ray -> r) tMax s = do
    isect@QuadricIntersection{..} <- intersectRayQuadric r tMax s
    pure $ RayIntersection (interactionFromQuadric r isect s) _qiTHit

  rayIntersects (view ray -> r) tMax = isJust . intersectRayQuadric r tMax

  surfaceArea Sphere{..} = _spherePhiMax * _sphereRadius * (_sphereZMax - _sphereZMin)

  sampleSurface u s@Sphere{..} = do
    guard $ detTransform _sphereToRender /= 0
    d <- sphereDistribution s
    let P (V2 cosθ ϕ) = realToFrac <$> sampleContinuous2_XY d (realToFrac <$> u)
    let sinθ = sqrt $ 1 - cosθ * cosθ
    let p = P $ _sphereRadius *^ sphericalDirection (SphericalCoordinates sinθ cosθ ϕ)
    let _ssPoint = (I.singleton <$> _sphereToRender) !!*!! I.vErr 5 p
    -- Compute surface normals at p
    let n
          | xor' _sphereFlipNormals _sphereTransformSwapsHandedness = negate $ _sphereToRender !!*!! N (unP p)
          | otherwise = _sphereToRender !!*!! N (unP p)
    -- Compute u, v coordinates at p
    let _ssParametricCoords = toParametric cosθ ϕ s
    pure
      SurfaceSample
        { _ssPoint
        , _ssNormal = n
        , _ssParametricCoords
        , _ssPdf = recip $ surfaceArea s
        , _ssTime = 0
        }

  surfacePdf _ = recip . surfaceArea

  sampleSurfaceFrom rp@ReferencePoint{..} u s@Sphere{..}
    -- If the observer is inside the sphere, sample any position on the sphere
    | squaredDistanceToCenter <= _sphereRadius * _sphereRadius = do
        SurfaceSample{..} <- sampleSurface u s
        -- vector from observer point to sample point
        let ωi = I.midpoint <$> _ssPoint .-. _rpPoint
        -- Squared distance of ωi
        let r = quadrance ωi
        -- If the distance is zero, the sample point is not valid.
        guard $ r > 0
        -- Transform the Area-based sphere PDF to a solid-angle-based PDF by
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
    -- Otherwise, sample a position on the visible portion of the sphere by
    -- sampling a direction within the cone subtended by sphere from the
    -- observer.
    | otherwise = do
        let sinθMax = _sphereRadius / sqrt squaredDistanceToCenter
        let sin2θMax = sinθMax * sinθMax
        let (sin2θ, complCosθMax, cosθ, ϕ)
              -- Angle is too small to use normal cone sampling, compute sample
              -- values via Taylor series expansion.
              | sin2θMax < smallSin2θMax =
                  (sin2θMax * u ^. _x, sin2θMax / 2, sqrt $ 1 - sin2θ, u ^. _y * 2 * pi)
              | otherwise =
                  let cosθMax = safeSqrt $ 1 - sin2θMax
                   in case fmap realToFrac $ sampleContinuous2_YX (uniformConeDistribution $ realToFrac cosθMax) $ realToFrac <$> u of
                        P (V2 cosθ' ϕ') -> (1 - cosθ' * cosθ', 1 - cosθMax, cosθ', ϕ')
        let cosα =
              sin2θ / sinθMax
                + cosθ * safeSqrt (1 - sin2θ / sin2θMax)
        let sinα = safeSqrt $ 1 - cosα * cosα
        let ω = sphericalDirection $ SphericalCoordinates sinα cosα ϕ
        let n = normalize $ N $ fromLocal (-ω) $ frameFromZ $ normalize ωc
        let p = pCenter + _sphereRadius *^ coerce n
        let _ssNormal
              | _sphereFlipNormals = -n
              | otherwise = n
        let _ssPoint = I.vErr 5 p
        let ϕSphere = atan2' (p ^. _y) (p ^. _x)
        let _ssParametricCoords = toParametric (p ^. _z / _sphereRadius) ϕSphere s
        guard $
          _ssParametricCoords ^. _x <= 0
            && _ssParametricCoords ^. _x >= 1
            && _ssParametricCoords ^. _y <= 0
            && _ssParametricCoords ^. _y >= 1
        pure
          SurfaceSample
            { _ssPoint
            , _ssNormal
            , _ssParametricCoords
            , _ssPdf = 1 / (2 * pi * complCosθMax)
            , _ssTime = _rpTime
            }
    where
      -- Point at the center of the sphere
      pCenter = _sphereToRender !!*!! origin
      -- Observer point, adjusted for error to ensure it is inside the sphere
      -- if it should be.
      pObserver = offsetRayOriginTo pCenter rp
      -- Vector from observer to origin of sphere.
      ωc = pCenter .-. pObserver
      squaredDistanceToCenter = quadrance ωc

  surfacePdfFrom rp@ReferencePoint{..} ωi s@Sphere{..}
    | squaredDistanceToCenter <= _sphereRadius * _sphereRadius = fromMaybe 0 do
        let r = spawnRay ωi rp
        QuadricIntersection{..} <- intersectRayQuadric r infinity s
        -- Transform the Area-based sphere PDF to a solid-angle-based PDF by
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
        pure $ squaredDistanceToPoint / (surfaceArea s * cosθ)
    | otherwise =
        let sin2θMax = _sphereRadius * _sphereRadius / squaredDistanceToCenter
            complCosθMax
              | sin2θMax < smallSin2θMax = sin2θMax / 2
              | otherwise = 1 - safeSqrt (1 - sin2θMax)
         in 1 / (2 * pi * complCosθMax)
    where
      -- Point at the center of the sphere
      pCenter = _sphereToRender !!*!! origin
      -- Observer point, adjusted for error to ensure it is inside the sphere
      -- if it should be.
      pObserver = offsetRayOriginTo pCenter rp
      -- Vector from observer to origin of sphere.
      ωc = pCenter .-. pObserver
      squaredDistanceToCenter = quadrance ωc

smallSin2θMax :: (Fractional a) => a
smallSin2θMax = 0.00068523

toParametric :: (Floating a, Ord a) => a -> a -> Sphere a -> Point V2 a
toParametric cosθ ϕ Sphere{..} = P $ V2 u v
  where
    θ = safeAcos cosθ
    u = ϕ / _spherePhiMax
    v = (θ - _sphereThetaMin) / (_sphereThetaMax - _sphereThetaMin)

sphere
  :: (Floating a, Ord a)
  => Transform a
  -> Transform a
  -> Bool
  -> a
  -> a
  -> a
  -> a
  -> Sphere a
sphere _sphereToRender _sphereFromRender _sphereFlipNormals (abs -> radius) zMin zMax phiMax =
  Sphere
    { _sphereRadius = radius
    , _sphereZMin
    , _sphereZMax
    , _sphereThetaMin = acos $ _sphereZMin / radius
    , _sphereThetaMax = acos $ _sphereZMax / radius
    , _spherePhiMax = clamp (0, 2 * pi) phiMax
    , _sphereFlipNormals
    , _sphereFromRender
    , _sphereToRender
    , _sphereTransformSwapsHandedness = swapsHandedness _sphereToRender
    }
  where
    _sphereZMin = clamp (-radius, radius) $ min zMin zMax
    _sphereZMax = clamp (-radius, radius) $ max zMin zMax

instance (Arbitrary a, IEEE a, Random a, Epsilon a) => Arbitrary (Sphere a) where
  arbitrary = do
    toRender <- arbitrary `suchThat` (not . nearZero . detTransform)
    isConcave <- arbitrary
    radius <- abs <$> arbitrary
    zMin <- choose (-radius, predIEEE radius)
    zMax <- choose (succIEEE zMin, radius)
    ϕMax <- choose (0, 2 * pi)
    pure $ sphere toRender (invTransform toRender) isConcave radius zMin zMax ϕMax
  shrink Sphere{..} =
    [ sphere
      toRender'
      (invTransform toRender')
      _sphereFlipNormals
      _sphereRadius
      _sphereZMin
      _sphereZMax
      _spherePhiMax
    | toRender' <- shrink _sphereToRender
    ]
      ++ [ sphere
          _sphereToRender
          (invTransform _sphereToRender)
          _sphereFlipNormals'
          _sphereRadius
          _sphereZMin
          _sphereZMax
          _spherePhiMax
         | _sphereFlipNormals' <- shrink _sphereFlipNormals
         ]
      ++ [ sphere
          _sphereToRender
          (invTransform _sphereToRender)
          _sphereFlipNormals
          _sphereRadius'
          _sphereZMin
          _sphereZMax
          _spherePhiMax
         | _sphereRadius' <- shrinkTowards 1 _sphereRadius
         ]
      ++ [ sphere
          _sphereToRender
          (invTransform _sphereToRender)
          _sphereFlipNormals
          _sphereRadius
          _sphereZMin'
          _sphereZMax
          _spherePhiMax
         | _sphereZMin' <- shrinkTowards (-_sphereRadius) _sphereZMin
         ]
      ++ [ sphere
          _sphereToRender
          (invTransform _sphereToRender)
          _sphereFlipNormals
          _sphereRadius
          _sphereZMin
          _sphereZMax'
          _spherePhiMax
         | _sphereZMax' <- shrinkTowards _sphereRadius _sphereZMax
         ]
      ++ [ sphere
          _sphereToRender
          (invTransform _sphereToRender)
          _sphereFlipNormals
          _sphereRadius
          _sphereZMin
          _sphereZMax
          _spherePhiMax'
         | _spherePhiMax' <- shrinkTowards (2 * pi) _spherePhiMax
         ]

shrinkTowards :: (Fractional a, Epsilon a, Eq a) => a -> a -> [a]
shrinkTowards target a
  | δ == 0 = []
  | nearZero δ = [target]
  | otherwise = [a + (δ * 0.5)]
  where
    δ = target - a

sphereRadius :: (Floating a, Ord a) => Lens' (Sphere a) a
sphereRadius = lens _sphereRadius \Sphere{..} radius ->
  let zMin = clamp (-radius, radius) _sphereZMin
      zMax = clamp (-radius, radius) _sphereZMax
   in Sphere
        { _sphereRadius = radius
        , _sphereThetaMin = acos $ zMin / radius
        , _sphereThetaMax = acos $ zMax / radius
        , ..
        }

sphereZMin :: (Floating a, Ord a) => Lens' (Sphere a) a
sphereZMin = lens _sphereRadius \Sphere{..} zMin ->
  let zMin' = clamp (-_sphereRadius, _sphereRadius) zMin
      zMax = max zMin' _sphereZMax
   in Sphere
        { _sphereThetaMin = acos $ zMin' / _sphereRadius
        , _sphereThetaMax = acos $ zMax / _sphereRadius
        , ..
        }

sphereZMax :: (Floating a, Ord a) => Lens' (Sphere a) a
sphereZMax = lens _sphereRadius \Sphere{..} zMax ->
  let zMax' = clamp (-_sphereRadius, _sphereRadius) zMax
      zMin = min zMax' _sphereZMin
   in Sphere
        { _sphereThetaMin = acos $ zMin / _sphereRadius
        , _sphereThetaMax = acos $ zMax' / _sphereRadius
        , ..
        }

sphereThetaMin :: (Floating a, Ord a) => Lens' (Sphere a) a
sphereThetaMin = lens _sphereRadius \Sphere{..} thetaMin ->
  let piOver2 = pi / 2
      thetaMin' = clamp (-piOver2, piOver2) thetaMin
      thetaMax = max thetaMin' _sphereThetaMax
   in Sphere
        { _sphereZMin = cos thetaMin' * _sphereRadius
        , _sphereZMax = cos thetaMax * _sphereRadius
        , ..
        }

sphereThetaMax :: (Floating a, Ord a) => Lens' (Sphere a) a
sphereThetaMax = lens _sphereRadius \Sphere{..} thetaMax ->
  let piOver2 = pi / 2
      thetaMax' = clamp (-piOver2, piOver2) thetaMax
      thetaMin = min thetaMax' _sphereThetaMin
   in Sphere
        { _sphereZMin = cos thetaMin * _sphereRadius
        , _sphereZMax = cos thetaMax' * _sphereRadius
        , ..
        }

spherePhiMax :: (Floating a, Ord a) => Lens' (Sphere a) a
spherePhiMax = lens _spherePhiMax \s phiMax ->
  s{_spherePhiMax = clamp (0, 2 * pi) phiMax}

sphereFlipNormals :: Lens' (Sphere a) Bool
sphereFlipNormals = lens _sphereFlipNormals \s _sphereFlipNormals -> s{_sphereFlipNormals}

data QuadricIntersection a = QuadricIntersection
  { _qiTHit :: a
  , _qiPObj :: Point V3 a
  , _qiPhi :: a
  }
  deriving (Eq, Ord, Show, Read, Functor)

intersectRayQuadric :: (IEEE a) => Ray a -> a -> Sphere a -> Maybe (QuadricIntersection a)
intersectRayQuadric Ray{..} tMax Sphere{..} = do
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
        let pHit' = I.midpoint <$> P (o .+^ t *^ d)
            P (V3 xHit yHit zHit) = pHit' ^* (_sphereRadius / norm pHit')
            xHit'
              | xHit == 0 && yHit == 0 = 1e-5 * _sphereRadius
              | otherwise = xHit
            pHit = P $ V3 xHit' yHit zHit
            phi = atan2' yHit xHit'
        guard $
          (_sphereZMin <= -_sphereRadius || zHit >= _sphereZMin)
            && (_sphereZMax >= _sphereRadius || zHit <= _sphereZMax)
            && phi <= _spherePhiMax
        pure $ QuadricIntersection (I.midpoint t) pHit phi
  validateHit tNear <|> validateHit tFar
  where
    fromRender = I.singleton <$> _sphereFromRender
    P o@(V3 ox oy oz) = fromRender !!*!! (I.singleton <$> _o)
    d@(V3 dx dy dz) = fromRender !!*!! (I.singleton <$> _d)
    a = I.square dx + I.square dy + I.square dz
    b = I.mulPow2 2 $ dx * ox + dy * oy + dz * oz
    ri = I.singleton _sphereRadius
    c = I.square ox + I.square oy + I.square oz - I.square ri
    v = o - b / (a + a) *^ d
    len = norm v
    discriminant = mulPow2 4 $ a * (ri + len) * (ri - len)

interactionFromQuadric :: (Epsilon a, IEEE a) => Ray a -> QuadricIntersection a -> Sphere a -> SurfaceInteraction a
interactionFromQuadric Ray{..} QuadricIntersection{..} Sphere{..} =
  _sphereToRender
    !!*!! surfaceInteraction
      siP
      _time
      (Just $ _sphereFromRender !!*!! (-_d))
      (P $ V2 u v)
      ( surfaceLocalGeometry
          dpdu
          dpdv
          dndu
          dndv
          (_sphereTransformSwapsHandedness `xor'` _sphereFlipNormals)
      )
  where
    P (V3 xHit yHit zHit) = _qiPObj
    siP = I.vErr 5 _qiPObj
    u = _qiPhi / _spherePhiMax
    cosTheta = zHit / _sphereRadius
    theta = safeAcos cosTheta
    thetaRange = _sphereThetaMax - _sphereThetaMin
    v = (theta - _sphereThetaMin) / thetaRange
    zProj = sqrt $ xHit * xHit + yHit * yHit
    cosPhi = xHit / zProj
    sinPhi = yHit / zProj
    sinTheta = safeSqrt $ 1 - cosTheta * cosTheta
    dpdu =
      V3
        (-_spherePhiMax * yHit)
        (_spherePhiMax * xHit)
        0
    dpdv =
      thetaRange
        *^ V3
          (zHit * cosPhi)
          (zHit * sinPhi)
          (-_sphereRadius * sinTheta)
    d²pdu² = -_spherePhiMax * _spherePhiMax *^ V3 xHit yHit 0
    d²pdvdv = thetaRange * zHit * _spherePhiMax *^ V3 (-sinPhi) cosPhi 0
    d²pdv² = -thetaRange * thetaRange *^ unP _qiPObj
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

sphereDistribution :: (RealFrac a) => Sphere a -> Maybe UniformSphereDistribution
sphereDistribution Sphere{..} = do
  -- Note: cos θ is inversely proportional to θ in the range [0, pi], and
  -- cos θ = z when converting from cartesian to polar.
  -- Therefore, cosθMin = zMax / radius  and cosθMax = zMin /radius (here, cosθMin denotes
  -- cos θ_min, NOT min (cos θ) - i.e. cosθMin will be greater than cosθMax).
  let cosθMax = _sphereZMin / _sphereRadius
  let cosθMin = _sphereZMax / _sphereRadius
  uniformPartialSphereDistributionE cosθMin cosθMax _spherePhiMax
