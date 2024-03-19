module Statistics.Distribution2.UniformDiskSpec where

import Data.Proxy (Proxy (..))
import Linear
import Linear.Affine
import Statistics.Distribution2
import Statistics.Distribution2.UniformDisk
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Classes.Statistics.Distribution (UniformVariable (..))
import Test.QuickCheck.Classes.Statistics.Distribution2

spec :: Spec
spec = do
  describe "UniformDisk" do
    distribution2Spec $ Proxy @UniformDiskDistribution
    prop "sample / invSample" \d (UniformVariable u) (UniformVariable v) ->
      let uv = P $ V2 u v
          x = sampleUniformDisk d uv
          uv' = invSampleUniformDisk d x
       in counterexample ("UV0: " <> show uv) $
            counterexample ("UV1: " <> show uv') $
              counterexample ("X: " <> show x) $
                nearZero $
                  uv - uv'
    prop "sampleXY radius >= innerRadius" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 radius _) = sampleContinuous2_XY @UniformDiskDistribution d (P (V2 u v))
       in radius >= udInnerRadius d
    prop "sampleXY radius <= 1" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 radius _) = sampleContinuous2_XY @UniformDiskDistribution d (P (V2 u v))
       in radius <= 1
    prop "sampleYX radius >= innerRadius" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 radius _) = sampleContinuous2_YX @UniformDiskDistribution d (P (V2 u v))
       in radius >= udInnerRadius d
    prop "sampleYX radius <= 1" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 radius _) = sampleContinuous2_YX @UniformDiskDistribution d (P (V2 u v))
       in radius <= 1
    prop "sampleXY θ >= 0" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ θ) = sampleContinuous2_XY @UniformDiskDistribution d (P (V2 u v))
       in θ >= 0
    prop "sampleXY θ <= θMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ θ) = sampleContinuous2_XY @UniformDiskDistribution d (P (V2 u v))
       in θ <= udθMax d
    prop "sampleYX θ >= 0" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ θ) = sampleContinuous2_YX @UniformDiskDistribution d (P (V2 u v))
       in θ >= 0
    prop "sampleYX θ <= θMax" \d (UniformVariable u) (UniformVariable v) ->
      let P (V2 _ θ) = sampleContinuous2_YX @UniformDiskDistribution d (P (V2 u v))
       in θ <= udθMax d
