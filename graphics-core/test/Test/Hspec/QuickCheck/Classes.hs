module Test.Hspec.QuickCheck.Classes where

import Data.Foldable (traverse_)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Classes

laws :: Laws -> Spec
laws Laws{..} = describe lawsTypeclass $ traverse_ (uncurry prop) lawsProperties
