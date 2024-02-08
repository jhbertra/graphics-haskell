{-# OPTIONS_GHC -Wno-orphans #-}

module Linear.Affine.Arbitrary where

import Linear.Affine (Point (..))
import Test.QuickCheck (Arbitrary)

deriving newtype instance (Arbitrary (f a)) => Arbitrary (Point f a)
