{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Geometry.Bounds where

import Control.Lens (Fold, Index, IxValue, Ixed (..), folding, makeLenses, (^.))
import Control.Lens.Traversal (Traversable1 (..))
import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Distributive (Distributive (..))
import Data.Foldable1 (Foldable1 (foldMap1))
import Data.Function (on)
import Data.Functor.Apply (Apply)
import Data.Functor.Bind (Apply (..))
import Data.Ix (Ix (range))
import Data.Kind (Type)
import Data.Vector.Generic (Vector (basicUnsafeSlice))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic, Generic1)
import Geometry.Ray (Ray (..))
import Linear (Additive (..), R1 (_x), R2 (_xy, _y), V1 (..), V2 (..), V3 (..), (^/), _xz, _yz)
import Linear.Affine (Affine (..), Point (..), distanceA, unP)
import Linear.Affine.Arbitrary ()
import Linear.Arbitrary ()
import Numeric.IEEE (IEEE (..))
import Test.QuickCheck (Arbitrary (..), elements, genericShrink, suchThat)
import Prelude hiding (null)

data Bounds f (a :: Type) = Bounds
  { _minP :: Point f a
  , _maxP :: Point f a
  }
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read, Foldable, Traversable, Data, Typeable)

$(makeLenses ''Bounds)

instance (Applicative f) => Applicative (Bounds f) where
  f <*> a = Bounds (_minP f <*> _minP a) (_maxP f <*> _maxP a)
  pure = singularity . pure
  {-# INLINE (<*>) #-}
  {-# INLINE pure #-}

instance (Apply f) => Apply (Bounds f) where
  f <.> a = Bounds (_minP f <.> _minP a) (_maxP f <.> _maxP a)
  {-# INLINE (<.>) #-}

instance (Distributive f) => Distributive (Bounds f) where
  distribute f = Bounds (collect _minP f) (collect _maxP f)
  {-# INLINE distribute #-}

instance (Foldable1 f) => Foldable1 (Bounds f) where
  foldMap1 f Bounds{..} = foldMap1 f (unP _minP) <> foldMap1 f (unP _maxP)
  {-# INLINE foldMap1 #-}

instance (Traversable1 f) => Traversable1 (Bounds f) where
  traverse1 f Bounds{..} = Bounds <$> (P <$> traverse1 f (unP _minP)) <.> (P <$> traverse1 f (unP _maxP))
  {-# INLINE traverse1 #-}

newtype instance U.MVector s (Bounds f a) = MV_Bounds (U.MVector s (Point f a, Point f a))
newtype instance U.Vector (Bounds f a) = V_Bounds (U.Vector (Point f a, Point f a))

instance (U.Unbox (f a)) => M.MVector U.MVector (Bounds f a) where
  basicLength = coerce $ M.basicLength @U.MVector @(Point f a, Point f a)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(Point f a, Point f a)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(Point f a, Point f a)
  basicUnsafeNew = fmap coerce . M.basicUnsafeNew @U.MVector @(Point f a, Point f a)
  basicInitialize = M.basicInitialize @U.MVector @(Point f a, Point f a) . coerce
  basicUnsafeCopy src dest = M.basicUnsafeCopy @U.MVector @(Point f a, Point f a) (coerce src) (coerce dest)
  basicUnsafeMove src dest = M.basicUnsafeMove @U.MVector @(Point f a, Point f a) (coerce src) (coerce dest)
  basicUnsafeGrow = (fmap . fmap) coerce . M.basicUnsafeGrow @U.MVector @(Point f a, Point f a) . coerce
  basicClear = fmap coerce . M.basicClear @U.MVector @(Point f a, Point f a) . coerce
  basicUnsafeReplicate n Bounds{..} = MV_Bounds <$> M.basicUnsafeReplicate n (_minP, _maxP)
  basicUnsafeRead (MV_Bounds v) i = uncurry Bounds <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Bounds v) i Bounds{..} = M.basicUnsafeWrite v i (_minP, _maxP)
  basicSet (MV_Bounds v) Bounds{..} = M.basicSet v (_minP, _maxP)
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}

instance (U.Unbox (f a)) => V.Vector U.Vector (Bounds f a) where
  basicUnsafeFreeze = fmap coerce . V.basicUnsafeFreeze @U.Vector @(Point f a, Point f a) . coerce
  basicUnsafeThaw = fmap coerce . V.basicUnsafeThaw @U.Vector @(Point f a, Point f a) . coerce
  basicLength = coerce $ V.basicLength @U.Vector @(Point f a, Point f a)
  basicUnsafeSlice = coerce $ V.basicUnsafeSlice @U.Vector @(Point f a, Point f a)
  basicUnsafeCopy src dest = V.basicUnsafeCopy @U.Vector @(Point f a, Point f a) (coerce src) (coerce dest)
  basicUnsafeIndexM (V_Bounds v) i = uncurry Bounds <$> V.basicUnsafeIndexM v i
  elemseq _ Bounds{..} z =
    V.elemseq (undefined :: U.Vector (Point f a)) _minP $
      V.elemseq (undefined :: U.Vector (Point f a)) _maxP z
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}

instance (Arbitrary (f a)) => Arbitrary (Bounds f a) where
  arbitrary = Bounds <$> arbitrary <*> arbitrary
  shrink = genericShrink

newtype NonDegenerateBounds f a = NonDegenerateBounds {unNonDegenerateBounds :: Bounds f a}
  deriving (Show)

instance (Additive f, Arbitrary (f a), Ord a) => Arbitrary (NonDegenerateBounds f a) where
  arbitrary = do
    p0 <- arbitrary
    NonDegenerateBounds . enclose p0 . singularity <$> arbitrary
  shrink (NonDegenerateBounds b) = do
    Bounds p0 p1 <- shrink b
    pure $ NonDegenerateBounds $ enclose p0 $ singularity p1

newtype NonNullBounds f a = NonNullBounds {unNonNullBounds :: Bounds f a}
  deriving (Show)

instance (Additive f, Arbitrary (f a), Ord a, Foldable f) => Arbitrary (NonNullBounds f a) where
  arbitrary = NonNullBounds . unNonDegenerateBounds <$> arbitrary `suchThat` (not . null . unNonDegenerateBounds)
  shrink (NonNullBounds b) = do
    NonDegenerateBounds b' <- shrink $ NonDegenerateBounds b
    guard $ not $ null b'
    pure $ NonNullBounds b'

instance (Additive f, Ord a) => Semigroup (Bounds f a) where
  (<>) = union
  {-# INLINE (<>) #-}

instance Bounded Float where
  minBound = -infinity
  maxBound = infinity

instance Bounded Double where
  minBound = -infinity
  maxBound = infinity

instance (Additive f, Ord a, Bounded a, Applicative f) => Monoid (Bounds f a) where
  mempty = nil
  {-# INLINE mempty #-}

instance Ixed (Bounds f a) where
  ix i = resolveExtreme i minP maxP

type instance Index (Bounds f a) = Extreme
type instance IxValue (Bounds f a) = Point f a

type Bounds1 = Bounds V1
type Bounds2 = Bounds V2
type Bounds3 = Bounds V3
type Bounds2f = Bounds2 Float
type Bounds2i = Bounds2 Int
type Bounds3f = Bounds3 Float
type Bounds3i = Bounds3 Int

data Extreme = Min | Max
  deriving (Ord, Eq, Generic, Show, Read, Enum, Bounded)

instance Arbitrary Extreme where
  arbitrary = elements [Min, Max]

resolveExtreme :: Extreme -> a -> a -> a
resolveExtreme Min a _ = a
resolveExtreme Max _ a = a
{-# INLINE resolveExtreme #-}

singularity :: Point f a -> Bounds f a
singularity a = Bounds a a
{-# INLINE singularity #-}

enclose :: (Additive f, Ord a) => Point f a -> Bounds f a -> Bounds f a
enclose = (<>) . singularity
{-# INLINE enclose #-}

intersect :: (Additive f, Ord a) => Bounds f a -> Bounds f a -> Bounds f a
intersect a b =
  Bounds
    (liftI2 max (_minP a) (_minP b))
    (liftI2 min (_maxP a) (_maxP b))
{-# INLINE intersect #-}

universal :: (Bounded a, Applicative f) => Bounds f a
universal = Bounds (pure minBound) (pure maxBound)
{-# INLINE universal #-}

union :: (Additive f, Ord a) => Bounds f a -> Bounds f a -> Bounds f a
union a b =
  Bounds
    (liftI2 min (_minP a) (_minP b))
    (liftI2 max (_maxP a) (_maxP b))
{-# INLINE union #-}

nil :: (Bounded a, Applicative f) => Bounds f a
nil = Bounds (pure maxBound) (pure minBound)
{-# INLINE nil #-}

overlaps :: (Ord a, Foldable f, Additive f) => Bounds f a -> Bounds f a -> Bool
overlaps a b = not $ degenerate $ intersect a b
{-# INLINE overlaps #-}

contains :: (Ord a, Additive f, Eq (f a)) => Bounds f a -> Bounds f a -> Bool
contains a b = union a b == a
{-# INLINE contains #-}

inside :: (Ord a, Foldable f, Additive f) => Point f a -> Bounds f a -> Bool
inside p b =
  and (liftI2 (>=) p (_minP b))
    && and (liftI2 (<=) p (_maxP b))
{-# INLINE inside #-}

insideExclusive :: (Ord a, Foldable f, Additive f) => Point f a -> Bounds f a -> Bool
insideExclusive p b =
  and (liftI2 (>=) p (_minP b))
    && and (liftI2 (<) p (_maxP b))
{-# INLINE insideExclusive #-}

distanceSquared :: forall f a. (Ord a, Foldable f, Additive f, Num a) => Point f a -> Bounds f a -> a
distanceSquared p b =
  sum $ liftI2 (uncurry . distanceSquared1D) p $ liftI2 (,) (_minP b) (_maxP b)
  where
    distanceSquared1D :: a -> a -> a -> a
    distanceSquared1D p' tMin tMax =
      let d = maximum [0, tMin - p', p' - tMax] in d * d
    {-# INLINE distanceSquared1D #-}
{-# INLINE distanceSquared #-}

distance :: forall f a. (Ord a, Foldable f, Additive f, Floating a) => Point f a -> Bounds f a -> a
distance = fmap sqrt . distanceSquared
{-# INLINE distance #-}

expand :: (Additive f, Num a, Applicative f) => a -> Bounds f a -> Bounds f a
expand d b = Bounds (_minP b ^-^ vd) (_maxP b ^+^ vd)
  where
    vd = pure d
{-# INLINE expand #-}

diagonal :: (Additive f, Num a) => Bounds f a -> f a
diagonal b = _maxP b .-. _minP b
{-# INLINE diagonal #-}

corner :: (Additive f, Applicative f) => f Extreme -> Bounds f a -> Point f a
corner extremes b = resolveExtreme <$> P extremes <*> _minP b <*> _maxP b
{-# INLINE corner #-}

corners2 :: Fold (Bounds2 a) (Point V2 a)
corners2 = folding \(Bounds (P (V2 x0 y0)) (P (V2 x1 y1))) ->
  [ P $ V2 x0 y0
  , P $ V2 x0 y1
  , P $ V2 x1 y0
  , P $ V2 x1 y1
  ]

corners3 :: Fold (Bounds3 a) (Point V3 a)
corners3 = folding \(Bounds (P (V3 x0 y0 z0)) (P (V3 x1 y1 z1))) ->
  [ P $ V3 x0 y0 z0
  , P $ V3 x0 y0 z1
  , P $ V3 x0 y1 z0
  , P $ V3 x0 y1 z1
  , P $ V3 x1 y0 z0
  , P $ V3 x1 y0 z1
  , P $ V3 x1 y1 z0
  , P $ V3 x1 y1 z1
  ]

surfaceArea :: (Num a) => Bounds3 a -> a
surfaceArea b =
  2 * sum (product . (d ^.) <$> [_xy, _xz, _yz])
  where
    d = diagonal b
{-# INLINE surfaceArea #-}

perimeter :: (Num a) => Bounds2 a -> a
perimeter b =
  2 * sum ((d ^.) <$> [_x, _y])
  where
    d = diagonal b
{-# INLINE perimeter #-}

lerpBounds
  :: forall f a
   . (Additive f, Num a, Applicative f)
  => f a
  -> Bounds f a
  -> Point f a
lerpBounds t b = lerp' <$> P t <*> _minP b <*> _maxP b
  where
    lerp' a u v = (1 - a) * u + a * v
    {-# INLINE lerp' #-}
{-# INLINE lerpBounds #-}

offset
  :: forall f a
   . (Additive f, Applicative f, Ord a, Fractional a)
  => Point f a
  -> Bounds f a
  -> f a
offset p b = offset' <$> o <*> unP (_minP b) <*> unP (_maxP b)
  where
    o = p .-. _minP b
    offset' o' min' max'
      | max' > min' = o' / (max' - min')
      | otherwise = 0
    {-# INLINE offset' #-}
{-# INLINE offset #-}

offsetI
  :: forall f a
   . (Additive f, Applicative f, Integral a)
  => Point f a
  -> Bounds f a
  -> f a
offsetI p b = offset' <$> o <*> unP (_minP b) <*> unP (_maxP b)
  where
    o = p .-. _minP b
    offset' o' min' max'
      | max' > min' = o' `div` (max' - min')
      | otherwise = 0
    {-# INLINE offset' #-}
{-# INLINE offsetI #-}

centroid :: (Additive f, Fractional a) => Bounds f a -> Point f a
centroid b = P $ on (^+^) unP (_maxP b) (_minP b) ^/ 2
{-# INLINE centroid #-}

centroidI :: (Additive f, Integral a) => Bounds f a -> Point f a
centroidI b = fmap (`div` 2) $ P $ on (^+^) unP (_maxP b) (_minP b)
{-# INLINE centroidI #-}

null :: (Foldable f, Additive f, Ord a) => Bounds f a -> Bool
null b = or $ liftI2 (>=) (_minP b) (_maxP b)
{-# INLINE null #-}

degenerate :: (Foldable f, Additive f, Ord a) => Bounds f a -> Bool
degenerate b = or $ liftI2 (>) (_minP b) (_maxP b)
{-# INLINE degenerate #-}

boundingSphere :: (Foldable f, Additive f, Ord a, Floating a) => Bounds f a -> (Point f a, a)
boundingSphere b
  | null b = (centroid b, 0)
  | otherwise = (c, distanceA (_maxP b) c)
  where
    c = centroid b
{-# INLINE boundingSphere #-}

pointsWithin :: (Ix (f a), Foldable f, Additive f, Ord a) => Bounds f a -> [Point f a]
pointsWithin b
  | null b = []
  | otherwise = range (_minP b, _maxP b)
{-# INLINE pointsWithin #-}

enclosed :: (Additive f, Foldable f, Num a, Ord a) => Bounds f a -> a
enclosed b
  | null b = 0
  | degenerate b = negate $ abs $ product $ diagonal b
  | otherwise = product $ diagonal b
{-# INLINE enclosed #-}

{-# SPECIALIZE rayIntersects' ::
  Point V3 Float -> V3 Float -> Float -> V3 Float -> Bounds3 Float -> Bool
  #-}
{-# SPECIALIZE rayIntersects' ::
  Point V3 Double -> V3 Double -> Double -> V3 Double -> Bounds3 Double -> Bool
  #-}
rayIntersects'
  :: (Ord a, Fractional a)
  => Point V3 a
  -> V3 a
  -> a
  -> V3 a
  -> Bounds3 a
  -> Bool
rayIntersects'
  (P (V3 ox oy oz))
  (V3 invDx invDy invDz)
  tRayMax
  (V3 sgnDx sgnDy sgnDz)
  (Bounds (P (V3 x0 y0 z0)) (P (V3 x1 y1 z1))) = do
    let txMin = resolveBound sgnDx x0 x1 ox invDx
    let txMax = resolveBound sgnDx x1 x0 ox invDx
    let tyMin = resolveBound sgnDy y0 y1 oy invDy
    let tyMax = resolveBound sgnDy y1 y0 oy invDy
    let tzMin = resolveBound sgnDz z0 z1 oz invDz
    let tzMax = resolveBound sgnDz z1 z0 oz invDz
    let tMin = max txMin $ max tyMin tzMin
    let tMax = min txMax $ min tyMax tzMax
    tMin <= tMax && tMin < tRayMax && tMax > 0
    where
      resolveBound sgn p0 p1 o' invD = ((sgn * p0 + (1 - sgn) * p1) - o') * invD
      {-# INLINE resolveBound #-}
{-# INLINE rayIntersects' #-}

intersectRay :: (RealFloat a) => Ray a -> a -> Bounds3 a -> Maybe (Bounds1 a)
intersectRay
  (Ray (P (V3 ox oy oz)) (V3 dx dy dz) _ _)
  tRayMax
  (Bounds (P (V3 x0 y0 z0)) (P (V3 x1 y1 z1))) = do
    let (txMin, txMax) = resolveBounds dx x0 x1 ox
    let (tyMin, tyMax) = resolveBounds dy y0 y1 oy
    let (tzMin, tzMax) = resolveBounds dz z0 z1 oz
    let tMin = max txMin $ max tyMin tzMin
    let tMax = min txMax $ min tyMax tzMax
    if tMin <= tMax && tMin < tRayMax && tMax > 0
      then Just $ Bounds (P (V1 tMin)) (P (V1 tMax))
      else Nothing
    where
      resolveBounds d o p0 p1
        | isNegativeZero d || d < 0 = ((p1 - o) * invD, (p0 - o) * invD)
        | otherwise = ((p0 - o) * invD, (p1 - o) * invD)
        where
          invD = recip d
      {-# INLINE resolveBounds #-}
{-# INLINE intersectRay #-}

rayIntersects :: (RealFloat a) => Ray a -> a -> Bounds3 a -> Bool
rayIntersects (Ray o (V3 (normalizeZero -> dx) (normalizeZero -> dy) (normalizeZero -> dz)) _ _) tMax =
  rayIntersects'
    o
    (V3 (recip dx) (recip dy) (recip dz))
    tMax
    ( V3
        (if isNegativeZero dx || dx < 0 then 0 else 1)
        (if isNegativeZero dx || dy < 0 then 0 else 1)
        (if isNegativeZero dz || dz < 0 then 0 else 1)
    )

normalizeZero :: (RealFloat a) => a -> a
normalizeZero x
  | isNegativeZero x = 0
  | otherwise = x
{-# INLINE rayIntersects #-}
