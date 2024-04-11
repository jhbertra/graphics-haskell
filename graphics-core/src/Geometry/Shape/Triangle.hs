{-# LANGUAGE StrictData #-}

module Geometry.Shape.Triangle (
  TriangleMesh,
  _tmVertexFaces,
  _tmVertexPositions,
  _tmVertexNormals,
  _tmVertexTangents,
  _tmVertexUVs,
  _tmFlipNormals,
  _tmTransformSwapsHandedness,
  triangleMesh,
  Triangle (..),
) where

import Control.Lens (Bifunctor (..), (^.))
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (clamp)
import qualified Data.Vector.Unboxed as U
import GHC.Float (double2Float, float2Double)
import Geometry.Barycentric
import qualified Geometry.Bounds as Bounds
import Geometry.Interaction (
  SurfaceInteraction (..),
  SurfaceLocalGeometry (..),
  setShadingGeometry,
  surfaceInteraction,
 )
import Geometry.Normal
import Geometry.Ray
import Geometry.Shape
import Geometry.Spherical (angleBetween, safeSqrt, sphericalTriangle, sphericalTriangleArea, toDirectionCone)
import Geometry.Transform (ApplyTransform (..), Transform, coordinateSystem, swapsHandedness)
import Linear
import Linear.Affine
import Numeric.FMA
import Numeric.IEEE (IEEE (..))
import qualified Numeric.Interval.IEEE as I
import Physics.Spectrum (lerpScalar)
import Statistics.Distribution2 (ContDistr2 (density2), sampleContinuous2_XY)
import Statistics.Distribution2.Bilinear (bilinearDistributionE)

data TriangleMesh = TriangleMesh
  { _tmVertexFaces :: U.Vector (Int, Int, Int)
  , _tmVertexPositions :: U.Vector (Point V3 Float)
  , _tmVertexNormals :: Maybe (U.Vector (Normal V3 Float))
  , _tmVertexTangents :: Maybe (U.Vector (V3 Float))
  , _tmVertexUVs :: Maybe (U.Vector (Point V2 Float))
  , _tmFlipNormals :: Bool
  , _tmTransformSwapsHandedness :: Bool
  }
  deriving (Show, Eq, Ord)

triangleMesh
  :: Transform Float
  -> U.Vector (Int, Int, Int)
  -> U.Vector (Point V3 Float)
  -> Maybe (U.Vector (Normal V3 Float))
  -> Maybe (U.Vector (V3 Float))
  -> Maybe (U.Vector (Point V2 Float))
  -> Bool
  -> TriangleMesh
triangleMesh toRender faces positions normals tangents uvs flipNormals =
  TriangleMesh
    faces
    (U.map (toRender !!*!!) positions)
    (U.map (flipNormal . (toRender !!*!!)) <$> normals)
    (U.map (toRender !!*!!) <$> tangents)
    uvs
    flipNormals
    transformSwapsHandedness
  where
    transformSwapsHandedness = swapsHandedness toRender
    flipNormal
      | flipNormals `xor'` transformSwapsHandedness = negate
      | otherwise = id

data Triangle = Triangle
  { _triangleMesh :: TriangleMesh
  , _triangleIndex :: Int
  }
  deriving (Show, Eq, Ord)

instance Shape Triangle Float where
  bounds t = foldMap Bounds.singularity [p0, p1, p2]
    where
      (p0, p1, p2) = vertexPositions t

  normalBounds t = toDirectionCone case ns of
    Just (n0, n1, n2) -> faceForward (unN $ sum [n0, n1, n2]) n
    Nothing
      | _tmFlipNormals `xor'` _tmTransformSwapsHandedness -> -n
      | otherwise -> n
    where
      TriangleMesh{..} = _triangleMesh t
      (p0, p1, p2) = vertexPositions t
      ns = vertexNormals t
      n = normalize $ N $ cross (p1 .-. p0) (p2 .-. p1)

  surfaceArea t = 0.5 * norm (cross (p1 .-. p0) (p2 .-. p1))
    where
      (p0, p1, p2) = vertexPositions t

  rayIntersects r tMax t = isJust $ intersectTriangle r tMax p0 p1 p2
    where
      (p0, p1, p2) = vertexPositions t

  intersectRay r tMax t = do
    let (p0, p1, p2) = vertexPositions t
    isect <- intersectTriangle r tMax p0 p1 p2
    pure $ toRayIntersection r isect t

  sampleSurface u t = do
    let area = surfaceArea t
    guard $ area > 0
    let _ssPdf = recip area
    let (p0, p1, p2) = vertexPositions t
    let b = sampleTriangleUniform u
    let p = berpF p0 p1 p2 b
    let n' = normalize $ N $ cross (p1 .-. p0) (p2 .-. p0)
    let TriangleMesh{..} = _triangleMesh t
    let _ssNormal = case vertexNormals t of
          Just (n0, n1, n2) -> faceForward (unN $ berpF n0 n1 n2 b) n'
          Nothing
            | _tmFlipNormals `xor'` _tmTransformSwapsHandedness -> -n'
            | otherwise -> n'
    let (uv0, uv1, uv2) = vertexUVs t
    let _ssParametricCoords = berpF uv0 uv1 uv2 b
    let _ssTime = 0
    let pErr = I.gamma 6 *^ berpF (abs p0) (abs p1) (abs p2) b
    let _ssPoint = I.fromMidpointAndMargin <$> p <*> pErr
    pure SurfaceSample{..}

  surfacePdf _ = recip . surfaceArea

  sampleSurfaceFrom rp@ReferencePoint{..} u t = do
    let p = I.midpoint <$> _rpPoint
    let sa = solidAngle p t
    if sa < minSphericalSampleArea || sa > maxSphericalSampleArea
      then sampleSurfaceUniformFrom rp u t
      else sampleSurfaceSphericalFrom rp p u t

  surfacePdfFrom rp@ReferencePoint{..} ωi t = do
    let p = I.midpoint <$> _rpPoint
    let sa = solidAngle p t
    if sa < minSphericalSampleArea || sa > maxSphericalSampleArea
      then surfacePdfUniformFrom rp p ωi t
      else case _rpNormals of
        Nothing -> recip sa
        Just (_, N ns) -> fromMaybe (recip sa) do
          let (p0, p1, p2) = vertexPositions t
          let ωi0 = normalize $ p0 .-. p
          let ωi1 = normalize $ p1 .-. p
          let ωi2 = normalize $ p2 .-. p
          distr <-
            bilinearDistributionE
              0
              0
              1
              1
              (float2Double $ max 0.01 $ abs $ dot ns ωi1)
              (float2Double $ max 0.01 $ abs $ dot ns ωi1)
              (float2Double $ max 0.01 $ abs $ dot ns ωi0)
              (float2Double $ max 0.01 $ abs $ dot ns ωi2)
          let u = invertSphericalTriangleSample p0 p1 p2 p ωi
          let P (V2 (float2Double -> ξ0) (float2Double -> ξ1)) = u
          pure $ double2Float (density2 distr ξ0 ξ1) / sa

sampleSurfaceSphericalFrom
  :: ReferencePoint Float -> Point V3 Float -> Point V2 Float -> Triangle -> Maybe (SurfaceSample Float)
sampleSurfaceSphericalFrom ReferencePoint{..} p u t = do
  let (p0, p1, p2) = vertexPositions t
  (pdfWarp, uWarp) <- case _rpNormals of
    Nothing -> pure (1, u)
    Just (_, N ns) -> do
      let ωi0 = normalize $ p0 .-. p
      let ωi1 = normalize $ p1 .-. p
      let ωi2 = normalize $ p2 .-. p
      distr <-
        bilinearDistributionE
          0
          0
          1
          1
          (float2Double $ max 0.01 $ abs $ dot ns ωi1)
          (float2Double $ max 0.01 $ abs $ dot ns ωi1)
          (float2Double $ max 0.01 $ abs $ dot ns ωi0)
          (float2Double $ max 0.01 $ abs $ dot ns ωi2)
      let P (V2 (float2Double -> ξ0) (float2Double -> ξ1)) = u
      pure
        ( double2Float $ density2 distr ξ0 ξ1
        , fmap double2Float $ sampleContinuous2_XY distr $ P $ V2 ξ0 ξ1
        )
  SphericalTriangleSample{..} <- sampleTriangleSpherical p0 p1 p2 p uWarp
  guard $ _stsPdf /= 0
  let n' = normalize $ N $ cross (p1 .-. p0) (p2 .-. p0)
  let TriangleMesh{..} = _triangleMesh t
  let _ssNormal = case vertexNormals t of
        Just (n0, n1, n2) -> faceForward (unN $ berpF n0 n1 n2 _stsB) n'
        Nothing
          | _tmFlipNormals `xor'` _tmTransformSwapsHandedness -> -n'
          | otherwise -> n'
  let (uv0, uv1, uv2) = vertexUVs t
  let _ssParametricCoords = berpF uv0 uv1 uv2 _stsB
  let _ssTime = _rpTime
  let pErr = I.gamma 6 *^ berpF (abs p0) (abs p1) (abs p2) _stsB
  let _ssPoint = I.fromMidpointAndMargin <$> berpF p0 p1 p2 _stsB <*> pErr
  let _ssPdf = pdfWarp * _stsPdf
  pure SurfaceSample{..}

sampleSurfaceUniformFrom :: ReferencePoint Float -> Point V2 Float -> Triangle -> Maybe (SurfaceSample Float)
sampleSurfaceUniformFrom ReferencePoint{..} u t = do
  SurfaceSample{..} <- sampleSurface u t
  let ωi = I.midpoint <$> _ssPoint .-. _rpPoint
  guard $ ωi > 0
  let ωi' = normalize ωi
  let pdf = _ssPdf * quadrance ωi / abs (dot (unN _ssNormal) (-ωi'))
  guard $ not $ isInfinite pdf
  pure
    SurfaceSample
      { _ssTime = _rpTime
      , _ssPdf = pdf
      , ..
      }

surfacePdfUniformFrom :: ReferencePoint Float -> Point V3 Float -> V3 Float -> Triangle -> Float
surfacePdfUniformFrom rp p ωi t = fromMaybe 0 do
  let r = spawnRay ωi rp
  RayIntersection{..} <- intersectRay r infinity t
  let SurfaceInteraction{..} = _riInteraction
  let p' = I.midpoint <$> _siPoint
  let N n = _surfaceNormal _siLocalGeometry
  let denom = surfaceArea t * abs (dot n (-ωi))
  guard $ denom > 0
  pure $ qdA p p' / denom

minSphericalSampleArea :: Float
minSphericalSampleArea = 3e-4

maxSphericalSampleArea :: Float
maxSphericalSampleArea = 6.22

sampleTriangleUniform :: (Ord a, Fractional a) => Point V2 a -> Barycentric a
sampleTriangleUniform (P (V2 ξ0 ξ1))
  | ξ0 < ξ1 =
      let b0 = ξ0 / 2
          b1 = ξ1 - b0
       in Barycentric b0 b1 $ 1 - ξ1
  | otherwise =
      let b1 = ξ1 / 2
          b0 = ξ0 - b1
       in Barycentric b0 b1 $ 1 - ξ0

data SphericalTriangleSample a = SphericalTriangleSample
  { _stsB :: Barycentric a
  , _stsPdf :: a
  }

sampleTriangleSpherical
  :: (IEEE a, Epsilon a, FMA a)
  => Point V3 a
  -> Point V3 a
  -> Point V3 a
  -> Point V3 a
  -> Point V2 a
  -> Maybe (SphericalTriangleSample a)
sampleTriangleSpherical p0 p1 p2 p (P (V2 ξ0 ξ1)) = do
  let a = normalize $ p0 .-. p
  let b = normalize $ p1 .-. p
  let c = normalize $ p2 .-. p
  let n_ab' = cross a b
  let n_bc' = cross b c
  let n_ca' = cross c a
  guard $ n_ab' /= 0 && n_bc' /= 0 && n_ca' /= 0
  let n_ab = normalize n_ab'
  let n_bc = normalize n_bc'
  let n_ca = normalize n_ca'
  let α = angleBetween n_ab (-n_ca)
  let β = angleBetween n_bc (-n_ab)
  let γ = angleBetween n_ca (-n_bc)
  let areaPlusPi = α + β + γ
  let area = areaPlusPi - pi
  guard $ area > 0
  let area'PlusPi = lerpScalar ξ0 pi areaPlusPi
  let cosα = cos α
  let sinα = sin α
  let sinϕ = differenceOfProducts (sin area'PlusPi) cosα (cos area'PlusPi) sinα
  let cosϕ = sumOfProducts (cos area'PlusPi) cosα (sin area'PlusPi) sinα
  let k1 = cosϕ + cosα
  let k2 = fma (-sinα) (dot a b) sinϕ
  let cosB' =
        clamp (-1, 1) $
          fma (differenceOfProducts k2 cosϕ k1 sinϕ) cosα k2
            / (sumOfProducts k2 sinϕ k1 cosϕ * sinα)
  let sinB' = safeSqrt $ 1 - cosB' * cosB'
  let c' = cosB' *^ a ^+^ sinB' *^ normalize (gramSchmidt c a)
  let cosθ = 1 - ξ1 * (1 - dot c' b)
  let sinθ = safeSqrt $ 1 - cosθ * cosθ
  let ω = cosθ *^ b + sinθ *^ normalize (gramSchmidt c' b)
  let e1 = p1 .-. p0
  let e2 = p2 .-. p0
  let s1 = cross ω e2
  let invDet = recip $ dot s1 e1
  let s = p .-. p0
  let b1' = clamp (0, 1) $ dot s s1 * invDet
  let b2' = clamp (0, 1) $ dot ω (cross s e1) * invDet
  let mag = b1' + b2'
  let (b1, b2)
        | mag > 1 = (b1' / mag, b2' / mag)
        | otherwise = (b1', b2')
  pure
    SphericalTriangleSample
      { _stsPdf = recip area
      , _stsB = Barycentric (1 - b1 - b2) b1 b2
      }

invertSphericalTriangleSample
  :: Point V3 Float
  -> Point V3 Float
  -> Point V3 Float
  -> Point V3 Float
  -> V3 Float
  -> Point V2 Float
invertSphericalTriangleSample p0 p1 p2 p ωi = fromMaybe 0 do
  let a = normalize $ p0 .-. p
  let b = normalize $ p1 .-. p
  let c = normalize $ p2 .-. p
  let n_ab' = cross a b
  let n_bc' = cross b c
  let n_ca' = cross c a
  guard $ n_ab' /= 0 && n_bc' /= 0 && n_ca' /= 0
  let n_ab = normalize n_ab'
  let n_bc = normalize n_bc'
  let n_ca = normalize n_ca'
  let α = angleBetween n_ab (-n_ca)
  let β = angleBetween n_bc (-n_ab)
  let γ = angleBetween n_ca (-n_bc)
  let c'' = normalize $ cross (cross b ωi) (cross c a)
  let c'
        | dot c'' (a ^+^ c) < 0 = -c''
        | otherwise = c''
  let mξ0
        | dot a c' > 0.99999847691 {- 0.1 deg -} = pure 0
        | otherwise = do
            let n_c'b' = cross c' b
            let n_ac'' = cross a c'
            guard $ quadrance n_c'b' > 0 && quadrance n_ac'' > 0
            let n_c'b = normalize n_c'b'
            let n_ac' = normalize n_ac''
            let area' = α + angleBetween n_ab n_c'b + angleBetween n_ac' (-n_c'b) - pi
            let area = α + β + γ - pi
            pure $ area' / area
  case mξ0 of
    Nothing -> pure 0.5
    Just ξ0 -> do
      pure $ fmap (clamp (0, 1)) $ P $ V2 ξ0 $ (1 - dot ωi b) / (1 - dot c' b)

gramSchmidt :: (Num a, Metric f) => f a -> f a -> f a
gramSchmidt v ŵ = v ^-^ dot v ŵ *^ ŵ

vertexPositions :: Triangle -> (Point V3 Float, Point V3 Float, Point V3 Float)
vertexPositions Triangle{..} =
  ( _tmVertexPositions U.! i
  , _tmVertexPositions U.! j
  , _tmVertexPositions U.! k
  )
  where
    TriangleMesh{..} = _triangleMesh
    (i, j, k) = _tmVertexFaces U.! _triangleIndex
{-# INLINE vertexPositions #-}

vertexNormals :: Triangle -> Maybe (Normal V3 Float, Normal V3 Float, Normal V3 Float)
vertexNormals Triangle{..} = do
  normals <- _tmVertexNormals
  pure (normals U.! i, normals U.! j, normals U.! k)
  where
    TriangleMesh{..} = _triangleMesh
    (i, j, k) = _tmVertexFaces U.! _triangleIndex
{-# INLINE vertexNormals #-}

vertexTangents :: Triangle -> Maybe (V3 Float, V3 Float, V3 Float)
vertexTangents Triangle{..} = do
  tangents <- _tmVertexTangents
  pure (tangents U.! i, tangents U.! j, tangents U.! k)
  where
    TriangleMesh{..} = _triangleMesh
    (i, j, k) = _tmVertexFaces U.! _triangleIndex
{-# INLINE vertexTangents #-}

vertexUVs :: Triangle -> (Point V2 Float, Point V2 Float, Point V2 Float)
vertexUVs Triangle{..} = case _tmVertexUVs of
  Nothing -> (P (V2 0 0), P (V2 1 0), P (V2 1 1))
  Just uvs -> (uvs U.! i, uvs U.! j, uvs U.! k)
  where
    TriangleMesh{..} = _triangleMesh
    (i, j, k) = _tmVertexFaces U.! _triangleIndex
{-# INLINE vertexUVs #-}

solidAngle :: Point V3 Float -> Triangle -> Float
solidAngle p t = do
  let (p0, p1, p2) = vertexPositions t
  sphericalTriangleArea $ sphericalTriangle (p0 .-. p) (p1 .-. p) (p2 .-. p)
{-# INLINE solidAngle #-}

xor' :: Bool -> Bool -> Bool
xor' False False = False
xor' False True = True
xor' True False = True
xor' True True = False

data TriangleIntersection = TriangleIntersection
  { _barycentric :: Barycentric Float
  , _t :: Float
  }

intersectTriangle
  :: Ray Float
  -> Float
  -> Point V3 Float
  -> Point V3 Float
  -> Point V3 Float
  -> Maybe TriangleIntersection
intersectTriangle Ray{..} tMax p0 p1 p2 = do
  guard $ not $ isDegenerateTriangle p0 p1 p2
  let kz = maxBasisIndex $ abs _d
  let kx = succ kz `mod` 3
  let ky = succ kx `mod` 3
  let V3 dx dy dz = permute kx ky kz _d
  let shearX = -dx / dz
  let shearY = -dy / dz
  let shearZ = recip dz
  let transformVertex p = P $ V3 (fma shearX z x) (fma shearY z y) (z * shearZ)
        where
          p' = p .-. _o
          V3 x y z = permute kx ky kz p'
      {-# INLINE transformVertex #-}
  let p0' = transformVertex p0
  let p1' = transformVertex p1
  let p2' = transformVertex p2
  let e0' = edge p1' p2'
  let e1' = edge p2' p0'
  let e2' = edge p0' p1'
  let (e0, e1, e2)
        | e0' /= 0 && e1' /= 0 && e2' /= 0 = (e0', e1', e2')
        | otherwise =
            ( double2Float $ on edge (fmap float2Double) p1' p2'
            , double2Float $ on edge (fmap float2Double) p2' p0'
            , double2Float $ on edge (fmap float2Double) p0' p1'
            )
  guard $ e0 >= 0 && e1 >= 0 && e2 >= 0 || e0 < 0 && e1 < 0 && e2 < 0
  let det = e0 + e1 + e2
  guard $ e0 + e1 + e2 /= 0
  let invDet = recip det
  let b = Barycentric e0 e1 e2 ^* invDet
  let t = berp (p0' ^. _z) (p1' ^. _z) (p2' ^. _z) b
  guard $ t > 0 && t <= tMax
  pure $ TriangleIntersection b t

edge :: (FMA a) => Point V3 a -> Point V3 a -> a
edge (P (V3 x0 y0 _)) (P (V3 x1 y1 _)) = differenceOfProducts x0 y1 x1 y0

permute :: Int -> Int -> Int -> V3 a -> V3 a
permute i j k
  | i == 0 && j == 1 && k == 2 = \(V3 x y z) -> V3 x y z
  | i == 1 && j == 2 && k == 0 = \(V3 x y z) -> V3 z x y
  | i == 2 && j == 0 && k == 1 = \(V3 x y z) -> V3 y z x
  | i == 0 && j == 2 && k == 1 = \(V3 x y z) -> V3 x z y
  | i == 2 && j == 1 && k == 0 = \(V3 x y z) -> V3 z y x
  | otherwise = \(V3 x y z) -> V3 y x z

maxBasisIndex :: V3 Float -> Int
maxBasisIndex (V3 x y z)
  | x >= y = if x >= z then 0 else 2
  | y >= z = 1
  | otherwise = 2

isDegenerateTriangle :: Point V3 Float -> Point V3 Float -> Point V3 Float -> Bool
isDegenerateTriangle a b c = quadrance (cross (c .-. a) (b .-. a)) == 0

toRayIntersection
  :: Ray Float
  -> TriangleIntersection
  -> Triangle
  -> RayIntersection Float
toRayIntersection (Ray{..}) TriangleIntersection{..} t =
  flip
    RayIntersection
    _t
    if isJust normals || isJust tangents
      then setShadingGeometry shadingGeometry True interaction
      else interaction
  where
    (p0, p1, p2) = vertexPositions t
    (uv0, uv1, uv2) = vertexUVs t
    duv02 = uv0 .-. uv2
    duv12 = uv1 .-. uv2
    dp02 = p0 .-. p2
    dp12 = p1 .-. p2
    det = differenceOfProducts (duv02 ^. _x) (duv12 ^. _y) (duv12 ^. _x) (duv02 ^. _y)
    (dpdu, dpdv)
      | nearZero det = computeDifferentialsDegenerate p0 p1 p2
      | otherwise =
          let invDet = recip det
              dpdu' = differenceOfProducts (pure $ duv12 ^. _y) dp02 (pure $ duv02 ^. _y) dp12 ^* invDet
              dpdv' = differenceOfProducts (pure $ duv02 ^. _x) dp12 (pure $ duv12 ^. _x) dp02 ^* invDet
           in if quadrance (cross dpdu' dpdv') == 0
                then computeDifferentialsDegenerate p0 p1 p2
                else (dpdu', dpdv')
    pHit = berpF p0 p1 p2 _barycentric
    uvHit = berpF uv0 uv1 uv2 _barycentric
    flipNormal = _tmFlipNormals (_triangleMesh t) `xor'` _tmTransformSwapsHandedness (_triangleMesh t)
    pAbsSum = berpF (abs p0) (abs p1) (abs p2) _barycentric
    pErr = I.gamma 7 *^ pAbsSum
    siP = I.fromMidpointAndMargin <$> pHit <*> pErr
    normal
      | flipNormal = negate $ N $ normalize $ cross dp02 dp12
      | otherwise = N $ normalize $ cross dp02 dp12
    interaction =
      surfaceInteraction
        siP
        _time
        (Just $ -_d)
        uvHit
        ( SurfaceLocalGeometry
            normal
            dpdu
            dpdv
            0
            0
        )
    normals = vertexNormals t
    tangents = vertexTangents t
    ns = case normals of
      Nothing -> normal
      Just (n0, n1, n2) ->
        let n' = berpF n0 n1 n2 _barycentric
         in if quadrance n' > 0
              then normalize n'
              else normal
    ss' = case tangents of
      Nothing -> dpdu
      Just (t0, t1, t2) ->
        let t' = berpF t0 t1 t2 _barycentric
         in if quadrance t' == 0
              then dpdu
              else t'
    ts' = cross (unN ns) ss'
    (ss, ts)
      | quadrance ts' > 0 = (cross ts' (unN ns), ts')
      | otherwise = coordinateSystem (unN ns)
    (dndu, dndv) = case normals of
      Nothing -> (0, 0)
      Just (n0, n1, n2)
        | nearZero det ->
            let dn = on cross unN (n2 ^-^ n0) (n1 ^-^ n0)
             in if quadrance dn == 0
                  then (0, 0)
                  else bimap N N $ coordinateSystem dn
        | otherwise ->
            let dn02 = n0 ^-^ n2
                dn12 = n1 ^-^ n2
                invDet = recip det
             in ( differenceOfProducts (pure $ duv12 ^. _y) dn02 (pure $ duv02 ^. _y) dn12 ^* invDet
                , differenceOfProducts (pure $ duv02 ^. _x) dn12 (pure $ duv12 ^. _x) dn02 ^* invDet
                )
    shadingGeometry = SurfaceLocalGeometry ns ss ts dndu dndv

computeDifferentialsDegenerate
  :: Point V3 Float
  -> Point V3 Float
  -> Point V3 Float
  -> (V3 Float, V3 Float)
computeDifferentialsDegenerate a b c = coordinateSystem $ normalize ng
  where
    ng' = cross (c .-. a) (b .-. a)
    ng
      | quadrance ng' == 0 = double2Float <$> on cross (fmap float2Double) (c .-. a) (b .-. a)
      | otherwise = ng'
