module Geometry.ShapeSpec where

import Control.Monad (guard)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Geometry.Bounds (normalizeZero)
import qualified Geometry.Bounds as Bounds
import Geometry.Ray (Ray (..))
import Geometry.Shape
import Geometry.Shape.Sphere (
  Sphere (_sphereRadius, _sphereToRender),
  shrinkTowards,
  sphereDistribution,
 )
import Geometry.Spherical (SphericalCoordinates (SphericalCoordinates), sphericalDirection)
import Geometry.Transform (ApplyTransform (..), detTransform)
import Linear
import Linear.Affine (Affine (..), Point (..), origin)
import Numeric.IEEE (IEEE (..))
import qualified Numeric.Interval.IEEE as I
import Statistics.Distribution2 (sampleContinuous2_XY)
import Statistics.Distribution2.UniformSphere (UniformSphereDistribution)
import System.Random (Random)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes (Laws (..))

spec :: Spec
spec = pure ()

shapeLaws
  :: forall s a
   . ( Show s
     , ArbitraryShape s a
     , Show a
     , Random a
     , Epsilon a
     , IEEE a
     )
  => Proxy s
  -> Laws
shapeLaws p =
  Laws
    "Shape"
    [ ("not bounds intersect => not intersects", shapeBoundIntersects p)
    , ("intersects => bounds intersect", shapeIntersectsBounds p)
    , ("genIntersecting intersects", genIntersectingIntersects p)
    , ("sample intersects", shapeIntersectSample p)
    , ("sample PDF", shapeSamplePdf p)
    ]

shapeBoundIntersects
  :: forall s a
   . ( Shape s a
     , Show s
     , Arbitrary s
     , Arbitrary a
     , Show a
     , RealFloat a
     )
  => Proxy s
  -> Property
shapeBoundIntersects _ = property \s ray (abs -> tMax) ->
  not (Bounds.rayIntersects ray tMax $ bounds s) ==>
    counterexample (show $ bounds s) $
      not (rayIntersects @s @a @Ray ray tMax s)

shapeIntersectsBounds
  :: forall s a
   . (ArbitraryShape s a, Show s, IEEE a, Show a)
  => Proxy s
  -> Property
shapeIntersectsBounds _ = property \s -> forAllIntersecting @s @a s \ray ->
  counterexample (show $ bounds s) $
    Bounds.rayIntersects ray infinity $
      bounds s

forAllIntersecting
  :: forall s a prop
   . (ArbitraryShape s a, Testable prop, Show a)
  => s
  -> (Ray a -> prop)
  -> Property
forAllIntersecting s f = case genIntersectingParams @s @a s of
  Nothing -> discard
  Just genParams -> forAllShrink genParams (shrinkIntersectingParams s) \params ->
    case rayFromIntersectingParams s params of
      Nothing -> discard
      Just ray -> counterexample (show ray) $ f ray

genIntersectingIntersects
  :: forall s a
   . (ArbitraryShape s a, Show s, IEEE a, Show a)
  => Proxy s
  -> Property
genIntersectingIntersects _ = property \s -> forAllIntersecting @s @a s \ray ->
  rayIntersects ray infinity s

shapeIntersectSample
  :: forall s a
   . ( Show s
     , ArbitraryShape s a
     , Show a
     , IEEE a
     , Epsilon a
     )
  => Proxy s
  -> Property
shapeIntersectSample _ = property \s (UniformVariable u) (UniformVariable v) ->
  case sampleSurface @s @a (P (V2 (realToFrac u) (realToFrac v))) s of
    Nothing -> discard
    Just SurfaceSample{..} ->
      forAllIntersecting @s @a s \(Ray o _ _) -> do
        let d = (I.midpoint <$> _ssPoint) .-. o
            ω = normalize d
            ray = Ray o ω 0
            err = abs $ norm d - norm (ω ^* norm d)
            sampleS = show SurfaceSample{..}
            rayS = show ray
            dS = show d
         in if nearZero err
              then
                counterexample sampleS $
                  counterexample rayS $
                    counterexample dS $
                      rayIntersects ray infinity s
              else discard

shapeSamplePdf
  :: forall s a
   . ( Shape s a
     , Show s
     , Arbitrary s
     , Random a
     , Show a
     , IEEE a
     , Epsilon a
     )
  => Proxy s
  -> Property
shapeSamplePdf _ = property \s -> do
  forAll (choose (0, 1)) \u ->
    case sampleSurface @s @a u s of
      Nothing -> discard
      Just ss@SurfaceSample{..} -> surfacePdf ss s == _ssPdf

class (Show (IntersectingParams s a), Shape s a, Arbitrary a, Arbitrary s) => ArbitraryShape s a where
  data IntersectingParams s a :: Type
  genIntersectingParams :: s -> Maybe (Gen (IntersectingParams s a))
  shrinkIntersectingParams :: s -> IntersectingParams s a -> [IntersectingParams s a]
  rayFromIntersectingParams :: s -> IntersectingParams s a -> Maybe (Ray a)

instance (Show a, IEEE a, Random a, Epsilon a, Bounded a, Arbitrary a) => ArbitraryShape (Sphere a) a where
  data IntersectingParams (Sphere a) a = SphereIntersectingParams
    { u :: UniformVariable
    , v :: UniformVariable
    , offset :: V3 a
    , dist :: UniformSphereDistribution
    }
  genIntersectingParams s = do
    let toRender = _sphereToRender s
    let detToRender = detTransform toRender
    let scaledArea = surfaceArea s * detToRender
    guard $ not $ nearZero scaledArea
    guard $ not $ nearZero $ recip scaledArea
    dist <- sphereDistribution s
    pure $
      SphereIntersectingParams
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary `suchThat` (\o -> not (nearZero o) && not (nearZero $ recip o))
        <*> pure dist
  shrinkIntersectingParams _ SphereIntersectingParams{..} =
    [SphereIntersectingParams{u = u', ..} | u' <- shrink u]
      <> [SphereIntersectingParams{v = v', ..} | v' <- shrink v]
      <> [SphereIntersectingParams{offset = offset', ..} | offset' <- shrinkTowards 1 offset]
  rayFromIntersectingParams
    s
    (SphereIntersectingParams (UniformVariable u) (UniformVariable v) offset dist) = do
      let toRender = _sphereToRender s
      let radius = _sphereRadius s
      let P (V2 cosθ ϕ) = realToFrac <$> sampleContinuous2_XY dist (P $ V2 u v)
      let θ = acos cosθ
      let sinθ = sin θ
      let vTarget = sphericalDirection $ SphericalCoordinates sinθ cosθ ϕ
      let target = toRender !!*!! (origin .+^ vTarget ^* radius)
      let normD = norm offset
      let d' = normalize offset
      let offset' = d' ^* normD
      let d = -d'
      let o = target .+^ offset'
      let ray = Ray o (normalizeZero <$> d) 0
      guard $ rayIntersects ray infinity s
      pure ray

deriving instance (Show a) => Show (IntersectingParams (Sphere a) a)

newtype UniformVariable = UniformVariable Double
  deriving (Show, Eq)

instance Arbitrary UniformVariable where
  arbitrary = UniformVariable <$> choose (0, 1 - epsilon)
  shrink (UniformVariable x) = case decodeFloat x of
    (_, 0) -> []
    (m, e) -> [UniformVariable $ encodeFloat m (e + (e + exponent1))]

exponent1 :: Int
exponent1 = case decodeFloat @Double 1 of
  (_, e) -> -e - 1
