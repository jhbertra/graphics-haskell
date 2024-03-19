{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

module Geometry.Spherical where

import Data.Data (Typeable)
import Data.Maybe (isNothing)
import Data.Ord (clamp)
import GHC.Base (Word16#)
import GHC.Word (Word16 (..))
import Geometry.Bounds (Bounds, boundingSphere)
import Linear
import Linear.Affine
import Numeric.IEEE (IEEE (..))

-- Spherical triangles

data SphericalTriangle a = UnsafeSphericalTriangle (V3 a) (V3 a) (V3 a)
  deriving (Ord, Eq, Show, Read, Foldable, Typeable)

sphericalTriangle :: (Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> SphericalTriangle a
sphericalTriangle a b c = UnsafeSphericalTriangle (normalize a) (normalize b) (normalize c)

atan2' :: (RealFloat a) => a -> a -> a
atan2' y x
  | ϕ < 0 = ϕ + 2 * pi
  | otherwise = ϕ
  where
    ϕ = atan2 y x

sphericalTriangleArea :: (RealFloat a) => SphericalTriangle a -> a
sphericalTriangleArea (UnsafeSphericalTriangle a b c) =
  abs $
    (2 *) $
      atan2
        (a `dot` (b `cross` c))
        (1 + (a `dot` b) + (a `dot` c) + (b `dot` c))

-- Spherical quads

data SphericalQuad a = UnsafeSphericalQuad (V3 a) (V3 a) (V3 a) (V3 a)
  deriving (Ord, Eq, Show, Read, Foldable, Typeable)

sphericalQuad :: (Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> V3 a -> SphericalQuad a
sphericalQuad a b c d = UnsafeSphericalQuad (normalize a) (normalize b) (normalize c) (normalize d)

sphericalQuadArea :: (RealFloat a) => SphericalQuad a -> a
sphericalQuadArea (UnsafeSphericalQuad a b c d) =
  sphericalTriangleArea (UnsafeSphericalTriangle a b c)
    + sphericalTriangleArea (UnsafeSphericalTriangle b c d)

-- Spherical coordinates

data SphericalCoordinates a = SphericalCoordinates
  { sinθ :: a
  , cosθ :: a
  , ϕ :: a
  }
  deriving (Ord, Eq, Show, Read, Foldable, Typeable)

sphericalDirection :: (Ord a, Floating a, Epsilon a) => SphericalCoordinates a -> V3 a
sphericalDirection SphericalCoordinates{..} =
  normalize $
    V3
      (clamp (-1, 1) sinθ * cos ϕ)
      (clamp (-1, 1) sinθ * sin ϕ)
      (clamp (-1, 1) cosθ)

safeAcos :: (Floating a, Ord a) => a -> a
safeAcos = acos . clamp (-1, 1)

safeSqrt :: (Floating a, Ord a) => a -> a
safeSqrt = sqrt . max 0

sphericalθ :: (Ord a, Floating a) => V3 a -> a
sphericalθ (V3 _ _ z) = safeAcos z

sphericalϕ :: (IEEE a) => V3 a -> a
sphericalϕ (V3 x y _) = fastSignBranch p p $ p + 2 * pi
  where
    p = atan2 y x

sphericalCosθ :: V3 a -> a
sphericalCosθ (V3 _ _ z) = z

sphericalCos²θ :: (Num a) => V3 a -> a
sphericalCos²θ (V3 _ _ z) = z * z

sphericalAbsCosθ :: (Num a) => V3 a -> a
sphericalAbsCosθ (V3 _ _ z) = abs z

sphericalSin²θ :: (Num a, Ord a) => V3 a -> a
sphericalSin²θ = max 0 . (1 -) . sphericalCos²θ

sphericalSinθ :: (Ord a, Floating a) => V3 a -> a
sphericalSinθ = sqrt . sphericalSin²θ

sphericalTan²θ :: (Ord a, Fractional a) => V3 a -> a
sphericalTan²θ v = sphericalSin²θ v / sphericalCos²θ v

sphericalTanθ :: (Ord a, Floating a) => V3 a -> a
sphericalTanθ v = sphericalSinθ v / sphericalCosθ v

sphericalCoordinates :: (IEEE a) => V3 a -> SphericalCoordinates a
sphericalCoordinates v =
  SphericalCoordinates (sphericalSinθ v) (sphericalCosθ v) (sphericalϕ v)

sphericalCosϕ :: (Ord a, Floating a) => V3 a -> a
sphericalCosϕ v@(V3 x _ _) = if sinθ == 0 then 1 else clamp (-1, 1) $ x / sinθ
  where
    sinθ = sphericalSinθ v

sphericalSinϕ :: (Ord a, Floating a) => V3 a -> a
sphericalSinϕ v@(V3 _ y _) = if sinθ == 0 then 0 else clamp (-1, 1) $ y / sinθ
  where
    sinθ = sphericalSinθ v

sphericalCosΔϕ :: (Ord a, Floating a) => V3 a -> V3 a -> a
sphericalCosΔϕ (V3 vx vy _) (V3 wx wy _) =
  if vxy == 0 || wxy == 0
    then 1
    else clamp (-1, 1) (vx * wx + vy * wy) / sqrt (vxy * wxy)
  where
    vxy = vx * vx + vy * vy
    wxy = wx * wx + wy * wy

-- Octahedral unit vector encoding

data OctahedralUnitVector = OctahedralUnitVector# Word16# Word16#
  deriving (Ord, Eq, Show, Typeable)

sign :: (IEEE a) => a -> a
sign z = copySign 1 (succIEEE z)
{-# INLINE sign #-}

signToFlag :: (IEEE a) => a -> a
signToFlag z = fromInteger $ (floor (sign z) + 1) `div` 2
{-# INLINE signToFlag #-}

fastBranch :: (Num a) => a -> a -> a -> a
fastBranch f x y = f * x + (1 - f) * y
{-# INLINE fastBranch #-}

fastSignBranch :: (IEEE a) => a -> a -> a -> a
fastSignBranch = fastBranch . signToFlag
{-# INLINE fastSignBranch #-}

encodeOctahedral :: V3 Float -> OctahedralUnitVector
encodeOctahedral v@(V3 x' y' z') = do
  let !(V3 !x !y !z) = v ^/ (abs x' + abs y' + abs z')
  let !flag = signToFlag z
  let !x'' = fastBranch flag x $ (1 - abs y) * sign x
  let !y'' = fastBranch flag y $ (1 - abs x) * sign y
  OctahedralUnitVector# (encode x'') (encode y'')
  where
    encode :: Float -> Word16#
    encode f = let !(W16# x) = round (clamp (0, 1) ((f + 1) / 2) * 65535) in x
    {-# INLINE encode #-}
{-# INLINE encodeOctahedral #-}

decodeOctahedral :: OctahedralUnitVector -> V3 Float
decodeOctahedral (OctahedralUnitVector# x y) = do
  let !x' = decode x
  let !y' = decode y
  let !z' = 1 - (abs x' + abs y')
  let !flag = signToFlag z'
  normalize $
    V3
      (fastBranch flag x' $ (1 - abs y') * sign x')
      (fastBranch flag y' $ (1 - abs x') * sign y')
      z'
  where
    decode :: Word16# -> Float
    decode i = (-1) + 2 * (fromIntegral (W16# i) / 65535)
    {-# INLINE decode #-}
{-# INLINE decodeOctahedral #-}

-- Area-preserving mapping

equalAreaSquareToSphere :: (IEEE a) => Point V2 a -> V3 a
equalAreaSquareToSphere (P (V2 x y)) = do
  let u = 2 * x - 1
  let v = 2 * y - 1
  let up = abs u
  let vp = abs v
  let signedDistance = 1 - (up + vp)
  let d = abs signedDistance
  let r = 1 - d
  let r² = r * r
  let ϕ = (if r == 0 then 1 else (vp - up) / r + 1) * pi / 4
  let z = copySign (1 - r²) signedDistance
  let cosϕ = copySign (cos ϕ) u
  let sinϕ = copySign (sin ϕ) v
  V3
    (cosϕ * r * safeSqrt (2 - r²))
    (sinϕ * r * safeSqrt (2 - r²))
    z
{-# INLINE equalAreaSquareToSphere #-}

equalAreaSphereToSquare :: (IEEE a) => V3 a -> Point V2 a
equalAreaSphereToSquare (V3 x y z) = do
  let x' = abs x
  let y' = abs y
  let z' = abs z
  let r = safeSqrt $ 1 - z'
  let a = max x y
  let b = min x y
  let b' = if a == 0 then 0 else b / a
  let ϕ = (if x' < y' then (1 -) else id) $ atan b' * 2 / pi
  let ϕr = ϕ * r
  let s = sign z
  let u = copySign (fastBranch s (r - ϕr) (1 - ϕr)) x
  let v = copySign (fastBranch s ϕr (1 - (r - ϕr))) y
  P $ V2 (0.5 * (u + 1)) $ 0.5 * (v + 1)
{-# INLINE equalAreaSphereToSquare #-}

wrapEqualEraSquare :: (IEEE a) => Point V2 a -> Point V2 a
wrapEqualEraSquare p = P $ wrapU p
  where
    wrapU (P (V2 u v))
      | u < 0 = wrapV (-u) (1 - v)
      | u > 1 = wrapV (2 - u) (1 - v)
      | otherwise = wrapV u v
    wrapV u v
      | v < 0 = V2 (1 - u) (-v)
      | v > 1 = V2 (1 - u) (2 - v)
      | otherwise = V2 u v
    {-# INLINE wrapU #-}
    {-# INLINE wrapV #-}
{-# INLINE wrapEqualEraSquare #-}

-- Bounding cones

data DirectionCone f a = UnsafeDirectionCone
  { dcDirection :: f a
  , dcCosθ :: Maybe a
  }
  deriving (Ord, Eq, Show, Read, Foldable, Typeable)

directionCone :: (Floating a, Metric f, Epsilon a) => f a -> a -> DirectionCone f a
directionCone direction cosθ = UnsafeDirectionCone (normalize direction) $ Just cosθ

toDirectionCone :: (Floating a, Metric f, Epsilon a) => f a -> DirectionCone f a
toDirectionCone f = directionCone f 1

allDirections :: (Additive f, Traversable f, Num a) => DirectionCone f a
allDirections = UnsafeDirectionCone (head basis) $ Just 0

nullDirectionCone :: DirectionCone f a -> Bool
nullDirectionCone UnsafeDirectionCone{..} = isNothing dcCosθ

inside :: (Metric f, Floating a, Epsilon a, Ord a) => f a -> DirectionCone f a -> Bool
inside w UnsafeDirectionCone{..} = maybe False inside' dcCosθ
  where
    inside' cosθ' = dot dcDirection (normalize w) >= cosθ'

subtend
  :: (Ord a, Floating a, Traversable f, Metric f, Epsilon a)
  => Point f a
  -> Bounds f a
  -> DirectionCone f a
subtend p b = do
  let (o, r) = boundingSphere b
  let r² = r * r
  let distance² = qdA p o
  if distance² < r²
    then allDirections
    else do
      let sin²θMax = r² / distance²
      let cosθMax = safeSqrt $ 1 - sin²θMax
      directionCone (o .-. p) cosθMax

union :: (IEEE a, Epsilon a, Conjugate a) => DirectionCone V3 a -> DirectionCone V3 a -> DirectionCone V3 a
union a b = case (dcCosθ a, dcCosθ b) of
  (Nothing, _) -> b
  (_, Nothing) -> a
  (Just cosθₐ, Just cosθᵦ) -> do
    let θₐ = safeAcos cosθₐ
    let θᵦ = safeAcos cosθᵦ
    let θ_d = angleBetween (dcDirection a) (dcDirection b)
    if
      | min (θ_d + θᵦ) pi <= θₐ -> a
      | min (θ_d + θₐ) pi <= θᵦ -> b
      | otherwise -> do
          let θₒ = θ_d + θₐ + θᵦ
          if θₒ >= pi
            then allDirections
            else do
              let θᵣ = θₒ - θₐ
              let wᵣ = dcDirection a `cross` dcDirection b
              if quadrance wᵣ == 0
                then allDirections
                else directionCone (rotate (axisAngle wᵣ θᵣ) (dcDirection a)) $ cos θₒ

angleBetween :: (IEEE a, Metric f) => f a -> f a -> a
angleBetween u v =
  fastSignBranch
    (dot u v)
    (2 * asin (norm (v ^-^ u) / 2))
    (pi - 2 * asin (norm (u ^+^ v) / 2))
