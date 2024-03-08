module Statistics.Distribution2.UniformSphereSpec where

import Data.Proxy (Proxy (..))
import Linear
import Linear.Affine
import Statistics.Distribution2
import Statistics.Distribution2.UniformSphere
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.QuickCheck.Classes (laws)
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Classes.Statistics.Distribution (UniformVariable (..), contDistrLaws, distributionLaws)
import Test.QuickCheck.Classes.Statistics.Distribution2

spec :: Spec
spec = do
  describe "UniformSphere" do
    laws $ distribution2Laws $ Proxy @UniformSphereDistribution
    laws $ contDistr2Laws $ Proxy @UniformSphereDistribution
    describe "Marginal2_X" do
      laws $ distributionLaws $ Proxy @(Marginal2_X UniformSphereDistribution)
      laws $ contDistrLaws $ Proxy @(Marginal2_X UniformSphereDistribution)
    describe "Marginal2_Y" do
      laws $ distributionLaws $ Proxy @(Marginal2_Y UniformSphereDistribution)
      laws $ contDistrLaws $ Proxy @(Marginal2_Y UniformSphereDistribution)
    describe "Conditional2_X" do
      laws $ distributionLaws $ Proxy @(Conditional2_X UniformSphereDistribution)
      laws $ contDistrLaws $ Proxy @(Conditional2_X UniformSphereDistribution)
    describe "Conditional2_Y" do
      laws $ distributionLaws $ Proxy @(Conditional2_Y UniformSphereDistribution)
      laws $ contDistrLaws $ Proxy @(Conditional2_Y UniformSphereDistribution)
    prop "sample / invSample XY" \d (UniformVariable u) (UniformVariable v) ->
      let x = P $ V2 u v
          x' = invSampleContinuous2_XY @UniformSphereDistribution d $ sampleContinuous2_XY d x
       in counterexample ("X: " <> show x) $
            counterexample ("X': " <> show x') $
              nearZero $
                x - x'
    prop "sample / invSample YX" \d (UniformVariable u) (UniformVariable v) ->
      let uv = P $ V2 u v
          x = sampleContinuous2_YX @UniformSphereDistribution d uv
          uv' = invSampleContinuous2_YX d x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
    prop "sample / invSample" \d (UniformVariable u) (UniformVariable v) ->
      let uv = P $ V2 u v
          x = sampleUniformSphere d uv
          uv' = invSampleUniformSphere d x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
    prop "sampleXY cosθ >= cosθMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 cosθ _) = sampleContinuous2_XY @UniformSphereDistribution d (P (V2 u v))
       in cosθ >= usCosθMax d
    prop "sampleXY cosθ <= 1" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 cosθ _) = sampleContinuous2_XY @UniformSphereDistribution d (P (V2 u v))
       in cosθ <= usCosθMin d
    prop "sampleYX cosθ >= cosθMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 cosθ _) = sampleContinuous2_YX @UniformSphereDistribution d (P (V2 u v))
       in cosθ >= usCosθMax d
    prop "sampleYX cosθ <= 1" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 cosθ _) = sampleContinuous2_YX @UniformSphereDistribution d (P (V2 u v))
       in cosθ <= usCosθMin d
    prop "sampleXY ϕ >= 0" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ ϕ) = sampleContinuous2_XY @UniformSphereDistribution d (P (V2 u v))
       in ϕ >= 0
    prop "sampleXY ϕ <= ϕMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ ϕ) = sampleContinuous2_XY @UniformSphereDistribution d (P (V2 u v))
       in ϕ <= usϕMax d
    prop "sampleYX ϕ >= 0" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ ϕ) = sampleContinuous2_YX @UniformSphereDistribution d (P (V2 u v))
       in ϕ >= 0
    prop "sampleYX ϕ <= ϕMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ ϕ) = sampleContinuous2_YX @UniformSphereDistribution d (P (V2 u v))
       in ϕ <= usϕMax d
