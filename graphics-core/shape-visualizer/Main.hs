module Main where

import Codec.Picture (PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (generateImage)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Ord (clamp)
import Data.Word (Word8)
import Geometry.Interaction
import Geometry.Normal
import Geometry.Ray
import Geometry.Shape
import Geometry.Shape.Cylinder (cylinder)
import Geometry.Shape.Sphere (sphere)
import Geometry.Transform
import Graphics.Color (RGB (..))
import Linear
import Linear.Affine (Point (..))
import qualified Numeric.Interval.IEEE as I

main :: IO ()
main = writePng "output.png" $ generateImage (render shape) res res
  where
    t = rotateX $ -pi / 2.5
    -- t = transform identity
    shape =
      sphere
        t
        (invTransform t)
        False
        1
        (-1)
        1
        (2 * pi)

res :: Int
res = 300

render :: (Shape s Float) => s -> Int -> Int -> PixelRGB8
render shape = \i j ->
  let x = zoom * (fromIntegral i / fromIntegral res * 2 - 1)
      y = -zoom * (fromIntegral j / fromIntegral res * 2 - 1)
      z = zoom
      rd = V3 0 0 (-1)
      ro = P (V3 x y z)
      r = Ray ro rd 0
      sampleColorDepth = do
        SurfaceSample{..} <- find (nearSample x y) samples
        let sz = case _ssPoint of P (V3 _ _ z') -> I.sup z' + 1e-2
        pure (RGB $ V3 _ssPdf 0 0, sz)
      colorDepth = do
        RayIntersection SurfaceInteraction{..} _ <- intersectRay r (1 / 0) shape
        let SurfaceLocalGeometry{..} = _siLocalGeometry
        pure
          ( lerp (max 0 $ dot (-lightDir) $ faceForward (-rd) _surfaceNormal) 1 0.01
          , case _siPoint of P (V3 _ _ iz) -> I.inf iz
          )
   in rgbToColor case sampleColorDepth of
        Nothing -> maybe 0 fst colorDepth
        Just (sampleColor, sampleDepth) -> case colorDepth of
          Nothing -> sampleColor
          Just (color, depth)
            | depth > sampleDepth -> color
            | otherwise -> sampleColor
  where
    zoom = 2
    rp =
      ReferencePoint
        { _rpPoint = P $ V3 1 0 $ I.singleton 0
        , _rpNormals = Nothing
        , _rpTime = 0
        }
    nearSample x y SurfaceSample{..} = case I.midpoint <$> _ssPoint of
      P (V3 x' y' _) ->
        abs (x - x') <= 2 / fromIntegral res
          && abs (y - y') <= 2 / fromIntegral res
    samples = catMaybes do
      u <- [0 .. 29]
      v <- [0 .. 29]
      pure $ sampleSurfaceFrom rp (P $ V2 (u / 30) (v / 30)) shape

lightDir :: Normal V3 Float
lightDir = normalize $ N $ V3 1 (-1) (-0.5)

rgbToColor :: RGB Float -> PixelRGB8
rgbToColor (RGB (V3 r g b)) = PixelRGB8 (gamma r) (gamma g) (gamma b)

gamma :: Float -> Word8
gamma = round . (* 255) . (** (1 / 2.2)) . clamp (0, 1)
