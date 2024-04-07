{-# LANGUAGE OverloadedLists #-}

module Main where

import Codec.Picture (PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (generateImage)
import Control.Monad (guard, replicateM)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Ord (clamp)
import Data.Word (Word8)
import GHC.IO (unsafePerformIO)
import qualified Geometry.Bounds as Bounds
import Geometry.Interaction
import Geometry.Normal
import Geometry.Ray
import Geometry.Shape
import Geometry.Shape.Triangle
import Geometry.Transform
import Graphics.Color (RGB (..))
import Linear
import Linear.Affine (Point (..))
import qualified Numeric.Interval.IEEE as I
import System.Random (randomRIO)

main :: IO ()
main = writePng "output.png" $ generateImage (render shape) res res
  where
    -- t = rotateZ $ pi / 4
    t = transform identity
    mesh =
      triangleMesh
        t
        [(0, 1, 2)]
        [P $ V3 0 0 0, P $ V3 1 0 0, P $ V3 0 1 0]
        (Just [N $ V3 0 0 (-1), N $ normalize $ V3 1 0 (-1), N $ normalize $ V3 0 1 (-1)])
        Nothing
        Nothing
        False
    shape = Triangle mesh 0

res :: Int
res = 600

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
        pure (RGB $ V3 _ssPdf 0 _ssPdf, sz)
      colorDepth = do
        guard $ Bounds.rayIntersects r (1 / 0) boundingBox
        RayIntersection SurfaceInteraction{..} _ <- intersectRay r (1 / 0) shape
        let SurfaceLocalGeometry{..} = _siShadingGeometry
        pure
          ( lerp (max 0 $ dot (-lightDir) $ faceForward (-rd) _surfaceNormal) 1 0.01
          , -- ( RGB $ unN _surfaceNormal
            case _siPoint of P (V3 _ _ iz) -> I.inf iz
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
        { _rpPoint = P $ V3 0 0 $ I.singleton $ -0.1
        , _rpNormals = Just (N $ V3 0 1 0, N $ V3 0 1 0)
        , _rpTime = 0
        }
    nearSample x y SurfaceSample{..} = case I.midpoint <$> _ssPoint of
      P (V3 x' y' _) ->
        abs (x - x') <= 2 / fromIntegral res
          && abs (y - y') <= 2 / fromIntegral res
    samples = catMaybes do
      (u, v) <- unsafePerformIO $ replicateM 300 $ randomRIO ((0, 0), (0.999999, 0.999999))
      pure $ sampleSurfaceFrom rp (P $ V2 u v) shape
    boundingBox = bounds shape

lightDir :: Normal V3 Float
lightDir = normalize $ N $ V3 1 (-1) (-0.5)

rgbToColor :: RGB Float -> PixelRGB8
rgbToColor (RGB (V3 r g b)) = PixelRGB8 (gamma r) (gamma g) (gamma b)

gamma :: Float -> Word8
gamma = round . (* 255) . (** (1 / 2.2)) . clamp (0, 1)
