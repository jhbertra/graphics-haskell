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
  describe "UniformHemisphere" do
    laws $ distribution2Laws $ Proxy @UniformHemisphereDistribution
    laws $ contDistr2Laws $ Proxy @UniformHemisphereDistribution
    describe "Marginal2_X" do
      laws $ distributionLaws $ Proxy @(Marginal2_X UniformHemisphereDistribution)
      laws $ contDistrLaws $ Proxy @(Marginal2_X UniformHemisphereDistribution)
    describe "Marginal2_Y" do
      laws $ distributionLaws $ Proxy @(Marginal2_Y UniformHemisphereDistribution)
      laws $ contDistrLaws $ Proxy @(Marginal2_Y UniformHemisphereDistribution)
    describe "Conditional2_X" do
      laws $ distributionLaws $ Proxy @(Conditional2_X UniformHemisphereDistribution)
      laws $ contDistrLaws $ Proxy @(Conditional2_X UniformHemisphereDistribution)
    describe "Conditional2_Y" do
      laws $ distributionLaws $ Proxy @(Conditional2_Y UniformHemisphereDistribution)
      laws $ contDistrLaws $ Proxy @(Conditional2_Y UniformHemisphereDistribution)
    prop "sample / invSample XY" \(UniformVariable u) (UniformVariable v) ->
      let d = UniformHemisphereDistribution
          x = P $ V2 u v
          x' = invSampleContinuous2_XY d $ sampleContinuous2_XY d x
       in counterexample ("X: " <> show x) $
            counterexample ("X': " <> show x) $
              nearZero $
                x - x'
    prop "sample / invSample YX" \(UniformVariable u) (UniformVariable v) ->
      let d = UniformHemisphereDistribution
          uv = P $ V2 u v
          x = sampleContinuous2_YX d uv
          uv' = invSampleContinuous2_YX d x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
    prop "sample / invSample" \(UniformVariable u) (UniformVariable v) ->
      let uv = P $ V2 u v
          x = sampleUniformHemisphere uv
          uv' = invSampleUniformHemisphere x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'

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
    prop "sample / invSample XY" \(UniformVariable u) (UniformVariable v) ->
      let d = UniformSphereDistribution
          x = P $ V2 u v
          x' = invSampleContinuous2_XY d $ sampleContinuous2_XY d x
       in counterexample ("X: " <> show x) $
            counterexample ("X': " <> show x') $
              nearZero $
                x - x'
    prop "sample / invSample YX" \(UniformVariable u) (UniformVariable v) ->
      let d = UniformSphereDistribution
          uv = P $ V2 u v
          x = sampleContinuous2_YX d uv
          uv' = invSampleContinuous2_YX d x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
    prop "sample / invSample" \(UniformVariable u) (UniformVariable v) ->
      let uv = P $ V2 u v
          x = sampleUniformSphere uv
          uv' = invSampleUniformSphere x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
