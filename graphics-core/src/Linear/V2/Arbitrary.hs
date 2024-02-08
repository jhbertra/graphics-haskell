{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.V2.Arbitrary (
  UnitV2 (..),
  NonZeroV2 (..),
  CartesianUnitV2 (..),
  BasisV2 (..),
) where

import Linear.Epsilon
import Linear.V2
import Linear.Vector

import GHC.Generics (Generic)
import System.Random (Random)
import Test.QuickCheck

-- | `Arbitrary V2` has no restrictions on components
instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

-- | `Arbitrary NonZero` is never the zero vector
newtype NonZeroV2 a = NonZeroV2 {unNonZeroV2 :: V2 a}
  deriving stock (Show, Generic)
  deriving newtype (Num, Epsilon)

instance (Arbitrary a, Epsilon a) => Arbitrary (NonZeroV2 a) where
  arbitrary = NonZeroV2 <$> arbitrary `suchThat` (not . nearZero)
  shrink = filter (not . nearZero) . genericShrink

-- | `Arbitrary UnitV2` always has norm 1
newtype UnitV2 a = UnitV2 {unUnitV2 :: V2 a} deriving (Show)

instance (Random a, Arbitrary a, Floating a) => Arbitrary (UnitV2 a) where
  arbitrary = do
    x <- choose (0, 1)
    pure $ UnitV2 $ V2 x $ sqrt $ 1 - x * x

-- | `Arbitrary CartesianUnitV2` is a unit vector along cartesian axis
newtype CartesianUnitV2 a = CartesianUnitV2 {unCartesianUnitV2 :: V2 a} deriving (Show)

instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (CartesianUnitV2 a) where
  arbitrary = elements $ CartesianUnitV2 <$> [unit _x, unit _y, -(unit _x), -(unit _y)]

-- | `Arbitrary BasisV2` is a orthonormal set of vectors
newtype BasisV2 a = BasisV2 {unBasisV2 :: (V2 a, V2 a)} deriving (Show)

instance (Arbitrary a, Floating a, Epsilon a, Random a) => Arbitrary (BasisV2 a) where
  arbitrary = do
    NonZeroV2 v0@(V2 x y) <- arbitrary
    s <- elements [1, -1]
    return $ BasisV2 (v0, s *^ V2 y (-x))
