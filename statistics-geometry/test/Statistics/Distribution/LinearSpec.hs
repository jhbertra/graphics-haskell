module Statistics.Distribution.LinearSpec where

import Data.Proxy (Proxy (..))
import Statistics.Distribution.Linear (LinearDistribution)
import Test.Hspec
import Test.Hspec.QuickCheck.Classes (laws)
import Test.QuickCheck.Classes.Statistics.Distribution (contDistrLaws, distributionLaws, meanLaws, varianceLaws)

spec :: Spec
spec = do
  laws $ distributionLaws $ Proxy @LinearDistribution
  laws $ contDistrLaws $ Proxy @LinearDistribution
  laws $ meanLaws $ Proxy @LinearDistribution
  laws $ varianceLaws $ Proxy @LinearDistribution
