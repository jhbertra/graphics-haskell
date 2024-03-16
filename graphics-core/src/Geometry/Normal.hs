module Geometry.Normal where

import Control.Lens (Field1 (..), Field2 (..), Field3 (..), coerced)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Data.Foldable1 (Foldable1)
import Data.Functor.Apply (Apply)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.V
import System.Random (Random)
import Test.QuickCheck

newtype Normal f a = N {unN :: f a}
  deriving stock (Show, Read, Eq, Ord, Generic, Traversable, Generic1)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadZip
    , Foldable
    , Eq1
    , Ord1
    , Additive
    , Affine
    , Metric
    , Finite
    , Apply
    , Foldable1
    , Semigroup
    , Monoid
    , Bounded
    , Num
    , Fractional
    , Floating
    , Ix
    , Epsilon
    , Random
    )

instance (Arbitrary (f a), Floating a, Metric f, Epsilon a, Epsilon (f a)) => Arbitrary (Normal f a) where
  arbitrary = N . normalize <$> arbitrary `suchThat` (not . nearZero)

instance (Field1 (f a) (f a) a a) => Field1 (Normal f a) (Normal f a) a a where
  _1 = coerced @_ @_ @(f a) @(f a) . _1

instance (Field2 (f a) (f a) a a) => Field2 (Normal f a) (Normal f a) a a where
  _2 = coerced @_ @_ @(f a) @(f a) . _2

instance (Field3 (f a) (f a) a a) => Field3 (Normal f a) (Normal f a) a a where
  _3 = coerced @_ @_ @(f a) @(f a) . _3

instance (R1 f) => R1 (Normal f) where
  _x = coerced . _x @f

instance (R2 f) => R2 (Normal f) where
  _y = coerced . _y @f
  _xy = coerced . _xy @f

instance (R3 f) => R3 (Normal f) where
  _z = coerced . _z @f
  _xyz = coerced . _xyz @f

faceForward :: (Num a, Ord a) => V3 a -> Normal V3 a -> Normal V3 a
faceForward v n
  | dot n (N v) < 0 = -n
  | otherwise = n
