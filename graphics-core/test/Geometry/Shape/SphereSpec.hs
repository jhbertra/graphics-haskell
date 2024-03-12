module Geometry.Shape.SphereSpec where

import Data.Proxy (Proxy (..))
import Geometry.Shape.Sphere (Sphere)
import Geometry.ShapeSpec (shapeLaws)
import Test.Hspec
import Test.Hspec.QuickCheck.Classes (laws)

spec :: Spec
spec = do
  laws $ shapeLaws $ Proxy @(Sphere Float)
