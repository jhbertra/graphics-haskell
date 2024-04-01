module Geometry.Normal where

import Control.Lens (Field1 (..), Field2 (..), Field3 (..), coerced)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Data.Foldable1 (Foldable1)
import Data.Functor.Apply (Apply)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Ix (Ix)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
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

newtype instance U.Vector (Normal f a) = V_N (U.Vector (f a))
newtype instance U.MVector s (Normal f a) = MV_N (U.MVector s (f a))
instance (U.Unbox (f a)) => U.Unbox (Normal f a)

instance (U.Unbox (f a)) => M.MVector U.MVector (Normal f a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_N v) = M.basicLength v
  basicUnsafeSlice m n (MV_N v) = MV_N (M.basicUnsafeSlice m n v)
  basicOverlaps (MV_N v) (MV_N u) = M.basicOverlaps v u
  basicUnsafeNew n = MV_N <$> M.basicUnsafeNew n
  basicUnsafeRead (MV_N v) i = N <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_N v) i (N x) = M.basicUnsafeWrite v i x
  basicInitialize (MV_N v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}

instance (U.Unbox (f a)) => G.Vector U.Vector (Normal f a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_N v) = V_N <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_N v) = MV_N <$> G.basicUnsafeThaw v
  basicLength (V_N v) = G.basicLength v
  basicUnsafeSlice m n (V_N v) = V_N (G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_N v) i = N <$> G.basicUnsafeIndexM v i

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
