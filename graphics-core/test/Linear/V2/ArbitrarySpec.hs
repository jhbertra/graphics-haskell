module Linear.V2.ArbitrarySpec where

import Linear
import Linear.Approximate
import Linear.V2.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "NonZeroV2" do
    prop "norm v > 0" \(NonZeroV2 v) ->
      counterexample ("Length: " <> show (norm v)) $ not $ nearZero @Float $ norm v
    prop "norm (shrink v) > 0" \v' -> case shrink v' of
      [] -> discard
      shrinks -> forAll (elements shrinks) \(NonZeroV2 v) ->
        counterexample ("Length: " <> show (norm v)) $ not $ nearZero @Float $ norm v
  describe "UnitV2" do
    prop "norm v ≡ 1" \(UnitV2 v) ->
      counterexample ("Length: " <> show (norm v)) $ norm v ==~ (1 :: Float)
  describe "CartesianUnitV2" do
    prop "norm v ≡ 1" \(CartesianUnitV2 v) ->
      counterexample ("Length: " <> show (norm v)) $ norm v === (1 :: Float)
    prop "x * y ≡ 0" \(CartesianUnitV2 (V2 x y)) ->
      x * y === (0 :: Float)
  describe "BasisV2" do
    prop "x ⋅ y ≡ 0" \(BasisV2 (x, y)) ->
      counterexample ("x ⋅ y: " <> show (dot x y)) $ dot x y === (0 :: Float)
