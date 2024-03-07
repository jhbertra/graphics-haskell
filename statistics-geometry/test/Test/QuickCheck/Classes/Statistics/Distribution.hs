module Test.QuickCheck.Classes.Statistics.Distribution where

import Data.Proxy (Proxy)
import Linear (Epsilon (..))
import Numeric.IEEE (IEEE (..))
import Statistics.Distribution
import Test.QuickCheck
import Test.QuickCheck.Classes

distributionLaws :: (Distribution d, Show d, Arbitrary d) => Proxy d -> Laws
distributionLaws p =
  Laws
    "Distribution"
    [ ("Lower bound", distributionLowerBound p)
    , ("Upper bound", distributionUpperBound p)
    , ("Complement", distributionComplement p)
    , ("Monotonicity", distributionMonotonicity p)
    ]

distributionLowerBound
  :: forall d
   . (Distribution d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionLowerBound _ = property \d ->
  cumulative @d d negInfinity === 0

distributionUpperBound
  :: forall d
   . (Distribution d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionUpperBound _ = property \d ->
  cumulative @d d infinity === 1

distributionComplement
  :: forall d
   . (Distribution d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionComplement _ = property \d x ->
  complCumulative @d d x === 1 - cumulative d x

distributionMonotonicity
  :: forall d
   . (Distribution d, Show d, Arbitrary d)
  => Proxy d
  -> Property
distributionMonotonicity _ = property \d x y ->
  let cdfX = cumulative @d d x
      cdfY = cumulative d y
   in counterexample ("CDF(X): " <> show cdfX) $
        counterexample ("CDF(Y): " <> show cdfY) case compare x y of
          LT -> cdfX <= cdfY
          EQ -> discard
          GT -> cdfX >= cdfY

negInfinity :: Double
negInfinity = negate infinity

contDistrLaws :: (ContDistr d, Show d, Arbitrary d) => Proxy d -> Laws
contDistrLaws p =
  Laws
    "ContDistr"
    [ ("Density / logDensity", contDistrDensityLogDensity p)
    , ("Non-negative", contDistrDensityNonNegative p)
    , ("Quantile inverse", contDistrDensityQuantileInverse p)
    , ("Quantile complement", contDistrDensityQuantileComplement p)
    ]

contDistrDensityLogDensity
  :: forall d
   . (ContDistr d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistrDensityLogDensity _ = property \d x ->
  log (density @d d x) === logDensity d x

contDistrDensityNonNegative
  :: forall d
   . (ContDistr d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistrDensityNonNegative _ = property \d x ->
  let pdf = density @d d x
   in counterexample ("PDF(X): " <> show pdf) $ pdf >= 0

-- | Integrate a function from negative infinity to a by using the change of variables @x = a + (t-1)/t@
--
-- This works /much/ better than just clipping the interval at some arbitrary small number.
fromNegInf :: ((Double -> Double) -> Double -> Double -> r) -> (Double -> Double) -> Double -> r
fromNegInf method f a = method (\t -> f (a + (t - 1) / t) / (t * t)) 0 1

contDistrDensityQuantileInverse
  :: forall d
   . (ContDistr d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistrDensityQuantileInverse _ = property \d (UniformVariable p) ->
  let x = quantile @d d p
      p' = cumulative d x
   in counterexample ("X: " <> show x) $
        counterexample ("CDF(X): " <> show p') $
          nearZero $
            p' - p

contDistrDensityQuantileComplement
  :: forall d
   . (ContDistr d, Show d, Arbitrary d)
  => Proxy d
  -> Property
contDistrDensityQuantileComplement _ = property \d (UniformVariable p) ->
  not (nearZero p) ==>
    let x = complQuantile @d d p
        x' = quantile d (1 - p)
     in counterexample ("X: " <> show x) $
          counterexample ("X': " <> show x') $
            nearZero $
              x' - x

meanLaws :: (Mean d, Show d, Arbitrary d) => Proxy d -> Laws
meanLaws p =
  Laws
    "Mean"
    [ ("Mean / MaybeMean", meanMaybeMean p)
    ]

meanMaybeMean
  :: forall d
   . (Mean d, Show d, Arbitrary d)
  => Proxy d
  -> Property
meanMaybeMean _ = property \d -> maybeMean @d d === Just (mean d)

varianceLaws :: (Variance d, Show d, Arbitrary d) => Proxy d -> Laws
varianceLaws p =
  Laws
    "Variance"
    [ ("Variance / MaybeVariance", varianceMaybeVariance p)
    ]

varianceMaybeVariance
  :: forall d
   . (Variance d, Show d, Arbitrary d)
  => Proxy d
  -> Property
varianceMaybeVariance _ = property \d -> maybeVariance @d d === Just (variance d)

newtype UniformVariable = UniformVariable Double
  deriving (Show, Eq)

instance Arbitrary UniformVariable where
  arbitrary = UniformVariable <$> choose (0, 1)
  shrink (UniformVariable x) = case decodeFloat x of
    (_, 0) -> []
    (m, e) -> [UniformVariable $ encodeFloat m (e + (e + exponent1))]

exponent1 :: Int
exponent1 = case decodeFloat @Double 1 of
  (_, e) -> -e - 1
