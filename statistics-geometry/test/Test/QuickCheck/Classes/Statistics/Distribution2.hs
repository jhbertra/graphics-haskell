module Test.QuickCheck.Classes.Statistics.Distribution2 where

import Data.Proxy (Proxy (..))
import Linear
import Linear.Affine
import Numeric.IEEE (IEEE (..))
import Statistics.Distribution2
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.QuickCheck.Classes
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Classes.Statistics.Distribution (UniformVariable (..), contDistrLaws, distributionLaws)

distribution2Spec
  :: forall d
   . ( ContDistr2 d
     , Show d
     , Arbitrary d
     , Show (Marginal2_X d)
     , Arbitrary (Marginal2_X d)
     , Show (Marginal2_Y d)
     , Arbitrary (Marginal2_Y d)
     , Show (Conditional2_X d)
     , Arbitrary (Conditional2_X d)
     , Show (Conditional2_Y d)
     , Arbitrary (Conditional2_Y d)
     )
  => Proxy d
  -> Spec
distribution2Spec p = do
  laws $ distribution2Laws p
  laws $ contDistr2Laws p
  describe "Marginal2_X" do
    laws $ distributionLaws $ Proxy @(Marginal2_X d)
    laws $ contDistrLaws $ Proxy @(Marginal2_X d)
  describe "Marginal2_Y" do
    laws $ distributionLaws $ Proxy @(Marginal2_Y d)
    laws $ contDistrLaws $ Proxy @(Marginal2_Y d)
  describe "Conditional2_X" do
    laws $ distributionLaws $ Proxy @(Conditional2_X d)
    laws $ contDistrLaws $ Proxy @(Conditional2_X d)
  describe "Conditional2_Y" do
    laws $ distributionLaws $ Proxy @(Conditional2_Y d)
    laws $ contDistrLaws $ Proxy @(Conditional2_Y d)
  prop "sample / invSample XY" \d (UniformVariable u) (UniformVariable v) ->
    let x = P $ V2 u v
        x' = invSampleContinuous2_XY @d d $ sampleContinuous2_XY d x
     in counterexample ("X: " <> show x) $
          counterexample ("X': " <> show x') $
            nearZero $
              x - x'
  prop "sample / invSample YX" \d (UniformVariable u) (UniformVariable v) ->
    let uv = P $ V2 u v
        x = sampleContinuous2_YX @d d uv
        uv' = invSampleContinuous2_YX d x
     in counterexample ("UV0: " <> show uv) $
          counterexample ("UV1: " <> show uv') $
            counterexample ("X: " <> show x) $
              nearZero $
                uv - uv'

distribution2Laws :: (Distribution2 d, Show d, Arbitrary d) => Proxy d -> Laws
distribution2Laws p =
  Laws
    "Distribution2"
    [ ("Lower bound", distributionLowerBound p)
    , ("Upper bound", distributionUpperBound p)
    , ("Complement", distributionComplement p)
    ]

distributionLowerBound
  :: forall d
   . (Distribution2 d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionLowerBound _ = property \d ->
  cumulative2 @d d negInfinity negInfinity === 0

distributionUpperBound
  :: forall d
   . (Distribution2 d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionUpperBound _ = property \d ->
  cumulative2 @d d infinity infinity === 1

distributionComplement
  :: forall d
   . (Distribution2 d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionComplement _ = property \d x y ->
  complCumulative2 @d d x y === 1 - cumulative2 d x y

negInfinity :: Double
negInfinity = negate infinity

contDistr2Laws :: (ContDistr2 d, Show d, Arbitrary d) => Proxy d -> Laws
contDistr2Laws p =
  Laws
    "ContDistr2"
    [ ("Density / logDensity", contDistr2DensityLogDensity p)
    , ("Non-negative", contDistr2DensityNonNegative p)
    ]

contDistr2DensityLogDensity
  :: forall d
   . (ContDistr2 d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistr2DensityLogDensity _ = property \d x y ->
  log (density2 @d d x y) === logDensity2 d x y

contDistr2DensityNonNegative
  :: forall d
   . (ContDistr2 d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistr2DensityNonNegative _ = property \d x y ->
  let pdf = density2 @d d x y
   in counterexample ("PDF(X, Y): " <> show pdf) $ pdf >= 0
