module Statistics.Distribution2.BilinearSpec where

import Data.Proxy (Proxy (..))
import Statistics.Distribution2.Bilinear (BilinearDistribution)
import Test.Hspec
import Test.QuickCheck.Classes.Statistics.Distribution2 (distribution2Spec)

spec :: Spec
spec = do
  distribution2Spec $ Proxy @BilinearDistribution
