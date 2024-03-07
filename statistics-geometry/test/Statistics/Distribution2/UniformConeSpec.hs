module Statistics.Distribution2.UniformConeSpec where

import Data.Proxy (Proxy (..))
import Linear
import Linear.Affine
import Statistics.Distribution2
import Statistics.Distribution2.UniformCone
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.QuickCheck.Classes (laws)
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Classes.Statistics.Distribution (UniformVariable (..), contDistrLaws, distributionLaws)
import Test.QuickCheck.Classes.Statistics.Distribution2

spec :: Spec
spec = do
  laws $ distribution2Laws $ Proxy @UniformConeDistribution
  laws $ contDistr2Laws $ Proxy @UniformConeDistribution
  describe "Marginal2_X" do
    laws $ distributionLaws $ Proxy @(Marginal2_X UniformConeDistribution)
    laws $ contDistrLaws $ Proxy @(Marginal2_X UniformConeDistribution)
  describe "Marginal2_Y" do
    laws $ distributionLaws $ Proxy @(Marginal2_Y UniformConeDistribution)
    laws $ contDistrLaws $ Proxy @(Marginal2_Y UniformConeDistribution)
  describe "Conditional2_X" do
    laws $ distributionLaws $ Proxy @(Conditional2_X UniformConeDistribution)
    laws $ contDistrLaws $ Proxy @(Conditional2_X UniformConeDistribution)
  describe "Conditional2_Y" do
    laws $ distributionLaws $ Proxy @(Conditional2_Y UniformConeDistribution)
    laws $ contDistrLaws $ Proxy @(Conditional2_Y UniformConeDistribution)
  prop "sample / invSample XY" \d (UniformVariable u) (UniformVariable v) ->
    let x = P $ V2 u v
        x' = invSampleContinuous2_XY @UniformConeDistribution d $ sampleContinuous2_XY d x
     in counterexample ("X: " <> show x) $
          counterexample ("X': " <> show x) $
            nearZero $
              x - x'
  prop "sample / invSample YX" \d (UniformVariable u) (UniformVariable v) ->
    let uv = P $ V2 u v
        x = sampleContinuous2_YX @UniformConeDistribution d uv
        uv' = invSampleContinuous2_YX d x
     in counterexample ("UV0: " <> show uv) $
          counterexample ("UV1: " <> show uv') $
            counterexample ("X: " <> show x) $
              nearZero $
                uv - uv'
  prop "sample / invSample" \(ucCosThetaMax -> cosθMax) (UniformVariable u) (UniformVariable v) ->
    let uv = P $ V2 u v
        x = sampleUniformCone cosθMax uv
        uv' = invSampleUniformCone cosθMax x
     in counterexample ("UV0: " <> show uv) $
          counterexample ("UV1: " <> show uv') $
            counterexample ("X: " <> show x) $
              nearZero $
                uv - uv'
  prop "sampleXY cosθ >= cosθMax" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 cosθ _) = sampleContinuous2_XY d (P (V2 u v))
     in cosθ >= ucCosThetaMax d
  prop "sampleXY cosθ <= 1" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 cosθ _) = sampleContinuous2_XY @UniformConeDistribution d (P (V2 u v))
     in cosθ <= 1
  prop "sampleYX cosθ >= cosθMax" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 cosθ _) = sampleContinuous2_YX d (P (V2 u v))
     in cosθ >= ucCosThetaMax d
  prop "sampleYX cosθ <= 1" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 cosθ _) = sampleContinuous2_YX @UniformConeDistribution d (P (V2 u v))
     in cosθ <= 1
  prop "sampleXY ϕ >= 0" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 _ ϕ) = sampleContinuous2_XY @UniformConeDistribution d (P (V2 u v))
     in ϕ >= 0
  prop "sampleXY ϕ <= 2pi" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 _ ϕ) = sampleContinuous2_XY @UniformConeDistribution d (P (V2 u v))
     in ϕ <= 2 * pi
  prop "sampleYX ϕ >= 0" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 _ ϕ) = sampleContinuous2_YX @UniformConeDistribution d (P (V2 u v))
     in ϕ >= 0
  prop "sampleYX ϕ <= 2pi" \d (UniformVariable u) (UniformVariable v) ->
    let P (V2 _ ϕ) = sampleContinuous2_YX @UniformConeDistribution d (P (V2 u v))
     in ϕ <= 2 * pi
