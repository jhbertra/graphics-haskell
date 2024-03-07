module Statistics.Distribution2.BilinearSpec where

import Data.Proxy (Proxy (..))
import Statistics.Distribution2.Bilinear (BilinearDistribution)
import Test.Hspec
import Test.Hspec.QuickCheck.Classes (laws)
import Test.QuickCheck.Classes.Statistics.Distribution2 (contDistr2Laws, distribution2Laws)

spec :: Spec
spec = do
  laws $ distribution2Laws $ Proxy @BilinearDistribution
  laws $ contDistr2Laws $ Proxy @BilinearDistribution
