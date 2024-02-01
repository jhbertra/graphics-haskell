{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Math.Vector where

import Control.Lens (Lens', lens, view, (^.))
import Data.Function (on)
import Data.Kind (Type)
import GHC.Base (Double (..), Double#, (*##), (+##), (-##))

data Vec2 = Vec2#
  { i2# :: Double#
  , j2# :: Double#
  }
  deriving (Show, Eq)

pattern Vec2 :: Double -> Double -> Vec2
pattern Vec2{i2, j2} <- Vec2# (D# -> i2) (D# -> j2)
  where
    Vec2 (D# i2#) (D# j2#) = Vec2#{..}

class Vector v where
  (^+) :: v -> v -> v
  (^*) :: Double -> v -> v
  (⋅) :: v -> v -> Double
  magSquared :: v -> Double
  magSquared v = v ⋅ v
  mag :: v -> Double
  mag = sqrt . magSquared
  normalize :: v -> v
  normalize v = v ^/ mag v
  (^/) :: v -> Double -> v
  v ^/ s = recip s ^* v
  zeroV :: v

class Dim1 v where
  type Element v :: Type
  i :: Lens' v (Element v)

instance Dim1 Double where
  type Element Double = Double
  i = id

instance Dim1 Vec2 where
  type Element Vec2 = Double
  i = lens (\Vec2#{..} -> D# i2#) \v (D# i2#) -> v{i2#}

class (Dim1 v) => Dim2 v where
  j :: Lens' v (Element v)

instance Dim2 Vec2 where
  j = lens (\Vec2#{..} -> D# j2#) \v (D# j2#) -> v{j2#}

instance Vector Vec2 where
  v0 ^+ v1 = Vec2# (i2# v0 +## i2# v1) (j2# v0 +## j2# v1)
  v0 ⋅ v1 = on (*) (view i) v0 v1 + on (*) (view j) v0 v1
  D# s ^* Vec2#{..} = Vec2# (s *## i2#) (s *## j2#)
  zeroV = Vec2# 0.0## 0.0##

data Point2 = Point2#
  { x2# :: Double#
  , y2# :: Double#
  }
  deriving (Show, Eq)

pattern Point2 :: Double -> Double -> Point2
pattern Point2{x2, y2} <- Point2# (D# -> x2) (D# -> y2)
  where
    Point2 (D# x2#) (D# y2#) = Point2#{..}

class (Vector (Vec p)) => Point p where
  type Vec p :: Type
  (.+) :: p -> Vec p -> p
  (.-) :: p -> p -> Vec p
  distSquared :: p -> p -> Double
  distSquared p0 p1 = magSquared $ p0 .- p1
  dist :: p -> p -> Double
  dist p0 p1 = mag $ p0 .- p1
  origin :: p
  toPoint :: Vec p -> p
  fromPoint :: p -> Vec p

instance Point Point2 where
  type Vec Point2 = Vec2
  p .+ v = Point2# (x2# p +## i2# v) (y2# p +## j2# v)
  p0 .- p1 = Vec2# (x2# p0 -## x2# p1) (y2# p0 -## y2# p1)
  origin = Point2# 0.0## 0.0##
  toPoint Vec2#{..} = Point2# i2# j2#
  fromPoint Point2#{..} = Vec2# x2# y2#

instance Dim1 Point2 where
  type Element Point2 = Double
  i = lens (\Point2#{..} -> D# x2#) \v (D# x2#) -> v{x2#}

instance Dim2 Point2 where
  j = lens (\Point2#{..} -> D# y2#) \v (D# y2#) -> v{y2#}

data Vec3 = Vec3#
  { i3# :: Double#
  , j3# :: Double#
  , k3# :: Double#
  }
  deriving (Show, Eq)

pattern Vec3 :: Double -> Double -> Double -> Vec3
pattern Vec3{i3, j3, k3} <- Vec3# (D# -> i3) (D# -> j3) (D# -> k3)
  where
    Vec3 (D# i3#) (D# j3#) (D# k3#) = Vec3#{..}

instance Dim1 Vec3 where
  type Element Vec3 = Double
  i = lens (\Vec3#{..} -> D# i3#) \v (D# i3#) -> v{i3#}

instance Dim2 Vec3 where
  j = lens (\Vec3#{..} -> D# j3#) \v (D# j3#) -> v{j3#}

class (Dim2 v) => Dim3 v where
  k :: Lens' v (Element v)

instance Dim3 Vec3 where
  k = lens (\Vec3#{..} -> D# k3#) \v (D# k3#) -> v{k3#}

instance Vector Vec3 where
  v0 ^+ v1 = Vec3# (i3# v0 +## i3# v1) (j3# v0 +## j3# v1) (k3# v0 +## k3# v1)
  v0 ⋅ v1 = on (*) (view i) v0 v1 + on (*) (view j) v0 v1 + on (*) (view k) v0 v1
  D# s ^* Vec3#{..} = Vec3# (s *## i3#) (s *## j3#) (s *## k3#)
  zeroV = Vec3# 0.0## 0.0## 0.0##

data Point3 = Point3#
  { x3# :: Double#
  , y3# :: Double#
  , z3# :: Double#
  }
  deriving (Show, Eq)

pattern Point3 :: Double -> Double -> Double -> Point3
pattern Point3{x3, y3, z3} <- Point3# (D# -> x3) (D# -> y3) (D# -> z3)
  where
    Point3 (D# x3#) (D# y3#) (D# z3#) = Point3#{..}

instance Point Point3 where
  type Vec Point3 = Vec3
  p .+ v = Point3# (x3# p +## i3# v) (y3# p +## j3# v) (z3# p +## k3# v)
  p0 .- p1 = Vec3# (x3# p0 -## x3# p1) (y3# p0 -## y3# p1) (z3# p0 -## z3# p1)
  origin = Point3# 0.0## 0.0## 0.0##
  toPoint Vec3#{..} = Point3# i3# j3# k3#
  fromPoint Point3#{..} = Vec3# x3# y3# z3#

instance Dim1 Point3 where
  type Element Point3 = Double
  i = lens (\Point3#{..} -> D# x3#) \v (D# x3#) -> v{x3#}

instance Dim2 Point3 where
  j = lens (\Point3#{..} -> D# y3#) \v (D# y3#) -> v{y3#}

instance Dim3 Point3 where
  k = lens (\Point3#{..} -> D# z3#) \v (D# z3#) -> v{z3#}

(×) :: Vec3 -> Vec3 -> Vec3
v0 × v1 =
  Vec3
    (v0 ^. j * v1 ^. k - v0 ^. k * v1 ^. j)
    (v0 ^. k * v1 ^. i - v0 ^. i * v1 ^. k)
    (v0 ^. i * v1 ^. j - v0 ^. j * v1 ^. i)
