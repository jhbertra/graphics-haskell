module Statistics.Distribution2.UniformSphereSpec where

import Data.Proxy (Proxy (..))
import Linear
import Linear.Affine
import Statistics.Distribution2
import Statistics.Distribution2.UniformSphere
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Classes.Statistics.Distribution (UniformVariable (..))
import Test.QuickCheck.Classes.Statistics.Distribution2

spec :: Spec
spec = do
  describe "UniformSphere" do
    distribution2Spec $ Proxy @UniformSphereDistribution
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
