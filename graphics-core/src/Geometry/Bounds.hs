{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Geometry.Bounds where

import Control.Lens (Index, IxValue, Ixed (..), makeLenses, (^.))
import Data.Function (on)
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)
import Linear (Additive (..), R1 (_x), R2 (_y), R3 (_z), V2, V3, (^/))
import Linear.Affine (Affine (..), Point (..), distanceA, unP)
import Prelude hiding (null)

data Bounds f (a :: Type) = Bounds
  { _minP :: Point f a
  , _maxP :: Point f a
  }
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

$(makeLenses ''Bounds)

data Extreme = Min | Max
  deriving (Ord, Eq, Generic, Show, Read, Enum, Bounded)

resolveExtreme :: Extreme -> a -> a -> a
resolveExtreme Min a _ = a
resolveExtreme Max _ a = a
{-# INLINE resolveExtreme #-}

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

instance (Additive f, Ord a) => Semigroup (Bounds f a) where
  (<>) = union
  {-# INLINE (<>) #-}

instance (Additive f, Ord a, Bounded a, Applicative f) => Monoid (Bounds f a) where
  mempty = nil
  {-# INLINE mempty #-}

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
overlaps a b =
  and (liftI2 (>=) (_maxP a) (_minP b))
    && and (liftI2 (<=) (_minP a) (_maxP b))
{-# INLINE overlaps #-}

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

area :: (Num a) => Bounds2 a -> a
area b = product $ (d ^.) <$> [_x, _y]
  where
    d = diagonal b
{-# INLINE area #-}

surfaceArea :: (Num a) => Bounds3 a -> a
surfaceArea b =
  2 * sum (product . fmap (d ^.) <$> [[_x, _y], [_x, _z], [_x, _y]])
  where
    d = diagonal b
{-# INLINE surfaceArea #-}

volume :: (Num a) => Bounds3 a -> a
volume b = product $ (d ^.) <$> [_x, _y, _z]
  where
    d = diagonal b
{-# INLINE volume #-}

lerpBounds
  :: forall f a
   . (Additive f, Num a, Applicative f)
  => f a
  -> Bounds f a
  -> Point f a
lerpBounds t b = lerp' <$> P t <*> _minP b <*> _maxP b
  where
    lerp' a u v = a * u + (1 - a) * v
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
