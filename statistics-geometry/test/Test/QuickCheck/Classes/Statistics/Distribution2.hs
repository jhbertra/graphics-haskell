module Test.QuickCheck.Classes.Statistics.Distribution2 where

import Data.Proxy (Proxy)
import Numeric.IEEE (IEEE (..))
import Statistics.Distribution2
import Test.QuickCheck
import Test.QuickCheck.Classes

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
