{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Geometry.Bounds where

import Control.Lens (Index, IxValue, Ixed (..), makeLenses, (^.))
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
import Linear (Additive (..), R1 (_x), R2 (_xy, _y), V2, V3, (^/), _xz, _yz)
import Linear.Affine (Affine (..), Point (..), distanceA, unP)
import Linear.Affine.Arbitrary ()
import Linear.Arbitrary ()
import Numeric.IEEE (IEEE (infinity))
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
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(Point f a, Point f a)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(Point f a, Point f a)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(Point f a, Point f a)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(Point f a, Point f a)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(Point f a, Point f a)
  basicClear = coerce $ M.basicClear @U.MVector @(Point f a, Point f a)
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
  basicUnsafeFreeze = coerce $ V.basicUnsafeFreeze @U.Vector @(Point f a, Point f a)
  basicUnsafeThaw = coerce $ V.basicUnsafeThaw @U.Vector @(Point f a, Point f a)
  basicLength = coerce $ V.basicLength @U.Vector @(Point f a, Point f a)
  basicUnsafeSlice = coerce $ V.basicUnsafeSlice @U.Vector @(Point f a, Point f a)
  basicUnsafeCopy = coerce $ V.basicUnsafeCopy @U.Vector @(Point f a, Point f a)
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

instance {-# OVERLAPPING #-} (Additive f, Applicative f) => Monoid (Bounds f Float) where
  mempty = nilIEEE

instance {-# OVERLAPPING #-} (Additive f, Applicative f) => Monoid (Bounds f Double) where
  mempty = nilIEEE

instance {-# OVERLAPPING #-} (Additive f, Ord a, Bounded a, Applicative f) => Monoid (Bounds f a) where
  mempty = nil
  {-# INLINE mempty #-}

instance Ixed (Bounds f a) where
  ix i = resolveExtreme i minP maxP

type instance Index (Bounds f a) = Extreme
type instance IxValue (Bounds f a) = Point f a

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

universalIEEE :: forall f a. (IEEE a, Applicative f) => Bounds f a
universalIEEE = Bounds (pure (-infinity)) $ pure infinity
{-# INLINE universalIEEE #-}

union :: (Additive f, Ord a) => Bounds f a -> Bounds f a -> Bounds f a
union a b =
  Bounds
    (liftI2 min (_minP a) (_minP b))
    (liftI2 max (_maxP a) (_maxP b))
{-# INLINE union #-}

nil :: (Bounded a, Applicative f) => Bounds f a
nil = Bounds (pure maxBound) (pure minBound)
{-# INLINE nil #-}

nilIEEE :: forall f a. (IEEE a, Applicative f) => Bounds f a
nilIEEE = Bounds (pure infinity) $ pure (-infinity)
{-# INLINE nilIEEE #-}

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
