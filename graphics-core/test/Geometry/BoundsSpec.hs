{-# LANGUAGE QuantifiedConstraints #-}

module Geometry.BoundsSpec where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Geometry.Bounds
import qualified Geometry.Bounds as Bounds
import Linear
import Linear.Approximate ((==~))
import Linear.V2.Arbitrary ()
import System.Random (Random)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.QuickCheck.Classes (laws)
import Test.QuickCheck (Arbitrary, choose, counterexample, elements, forAll, (===), (==>))
import Test.QuickCheck.Classes (
  applicativeLaws,
  eqLaws,
  foldableLaws,
  functorLaws,
  monoidLaws,
  ordLaws,
  semigroupLaws,
  showLaws,
  showReadLaws,
  traversableLaws,
 )
import Type.Reflection (Typeable, typeRep)

spec :: Spec
spec = do
  concreteBoundsSpec @V2 @Int do
    prop "lerpBounds / offsetI" \f (NonNullBounds b) -> do
      let p = lerpBounds @V2 @Int f b
      counterexample (show p) $ offsetI p b === f
    prop "centroidI inside" \(NonDegenerateBounds b) -> inside (centroidI @V2 @Int b) b
    prop "pointsWithin inside" \(NonNullBounds b) ->
      forAll (elements $ pointsWithin @V2 @Int b) (`inside` b)
  concreteBoundsSpec @V2 @Float do
    prop "lerpBounds / offset" \f (NonNullBounds b) -> do
      let p = lerpBounds @V2 @Float f b
      counterexample (show p) $ offset p b ==~ f
    prop "centroid inside" \(NonDegenerateBounds b) -> inside (centroid @V2 @Float b) b
  concreteBoundsSpec @V3 @Int do
    prop "lerpBounds / offsetI" \f (NonNullBounds b) -> do
      let p = lerpBounds @V3 @Int f b
      counterexample (show p) $ offsetI p b === f
    prop "centroidI inside" \(NonDegenerateBounds b) -> inside (centroidI @V3 @Int b) b
    prop "pointsWithin inside" \(NonNullBounds b) ->
      forAll (elements $ pointsWithin @V3 @Int b) (`inside` b)
  concreteBoundsSpec @V3 @Float do
    prop "lerpBounds / offset" \f (NonNullBounds b) -> do
      let p = lerpBounds @V3 @Float f b
      counterexample (show p) $ offset p b ==~ f
    prop "centroid inside" \(NonDegenerateBounds b) -> inside (centroid @V3 @Float b) b

concreteBoundsSpec
  :: forall (f :: Type -> Type) (a :: Type)
   . ( Typeable f
     , Typeable a
     , Show a
     , Read a
     , Arbitrary a
     , Bounded a
     , forall x. (Read x) => Read (f x)
     , forall x. (Show x) => Show (f x)
     , forall x. (Eq x) => Eq (f x)
     , forall x. (Ord x) => Ord (f x)
     , forall x. (Arbitrary x) => Arbitrary (f x)
     , Traversable f
     , Applicative f
     , Additive f
     , Num a
     , Ord a
     , Random a
     )
  => Spec
  -> Spec
concreteBoundsSpec extraSpec = describe (show $ typeRep @(Bounds f a)) do
  let bounds = Proxy @(Bounds f a)
  let boundsF = Proxy @(Bounds f)
  laws $ eqLaws bounds
  laws $ ordLaws bounds
  laws $ functorLaws boundsF
  laws $ showLaws bounds
  laws $ showReadLaws bounds
  laws $ foldableLaws boundsF
  laws $ traversableLaws boundsF
  laws $ applicativeLaws boundsF
  laws $ semigroupLaws bounds
  laws $ monoidLaws bounds
  prop "null singularity" $ Bounds.null . singularity @f @a
  prop "enclosed singularity" \p -> enclosed (singularity @f @a p) === 0
  prop "degenerate mempty" $ Bounds.degenerate $ mempty @(Bounds f a)
  prop "degenerate / null" \b -> Bounds.degenerate @f @a b ==> Bounds.null b
  prop "enclosed" \b -> counterexample
    ("enclosed b: " <> show (enclosed b))
    case compare (enclosed b) 0 of
      LT -> Bounds.degenerate @f @a b
      EQ -> Bounds.null b
      GT -> not $ Bounds.null b
  describe "union" do
    prop "commutative" \a b -> Bounds.union @f @a a b === Bounds.union b a
    prop "associative" \a b c -> Bounds.union @f @a a (Bounds.union b c) === Bounds.union (Bounds.union a b) c
    prop "distributive" \a b c ->
      Bounds.union @f @a a (Bounds.intersect b c)
        === (Bounds.union a b `Bounds.intersect` Bounds.union a c)
    prop "identity" \a -> Bounds.union @f @a a mempty === a
    prop "annulment" \a -> Bounds.union @f @a a universal === universal
    prop "idempotent" \a -> Bounds.union @f @a a a === a
  describe "intersect" do
    prop "commutative" \a b -> Bounds.intersect @f @a a b === Bounds.intersect b a
    prop "associative" \a b c -> Bounds.intersect @f @a a (Bounds.intersect b c) === Bounds.intersect (Bounds.intersect a b) c
    prop "distributive" \a b c ->
      Bounds.intersect @f @a a (Bounds.union b c)
        === (Bounds.intersect a b `Bounds.union` Bounds.intersect a c)
    prop "annulment" \a -> Bounds.intersect @f @a a mempty === mempty
    prop "identity" \a -> Bounds.intersect @f @a a universal === a
    prop "idempotent" \a -> Bounds.intersect @f @a a a === a
  prop "overlaps ==> not degenerate" \a b ->
    counterexample ("intersect a b: " <> show (Bounds.intersect a b)) $
      Bounds.overlaps a b === not (Bounds.degenerate @f @a $ Bounds.intersect a b)
  prop "distanceSquared / inside" \(NonDegenerateBounds b) ->
    forAll (traverse choose $ liftI2 (,) (_minP b) (_maxP b)) \p ->
      Bounds.distanceSquared @f @a p b === 0
  prop "inside / enclose" \a (NonDegenerateBounds b) ->
    counterexample ("enclose a b: " <> show (Bounds.enclose @f @a a b)) $
      Bounds.inside a b === (Bounds.enclose a b == b)
  prop "expand / contains" \a b -> do
    let b' = Bounds.expand @f @a a b
    counterexample ("expand a b: " <> show b') $
      if a < 0
        then Bounds.contains b b'
        else Bounds.contains b' b
  extraSpec
