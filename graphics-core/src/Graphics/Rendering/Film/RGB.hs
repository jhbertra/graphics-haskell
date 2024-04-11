module Graphics.Rendering.Film.RGB (
  RGBFilm,
) where

import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Geometry.Bounds as Bounds
import Graphics.Color
import Graphics.Rendering.Film
import Graphics.Rendering.Filter
import Graphics.Rendering.Sensor (sensorResponse)
import Linear
import Linear.Affine (Affine (..), Point (..))
import Numeric.FMA

data RGBFilm c p a = RGBFilm
  { _rgbFilmCommon :: FilmCommon p a
  , _rgbFilmColorSpace :: c
  , _rgbFilmMaxComponentValue :: a
  , _rgbFilmOutputRBGFromSensorRGB :: M33 a
  , _rgbFilmPixels :: Vector (Vector (RGBPixel a))
  , _rgbFilmFilterIntegral :: a
  }

data RGBPixel a = RGBPixel
  { rgbSum :: IORef (RGB a)
  , weightSum :: IORef a
  , splatSum :: IORef (RGB a)
  }

instance HasFilmCommon (RGBFilm c p a) p a where
  getFilmCommon = _rgbFilmCommon

instance (Filter p a, RealFloat a, FMA a, RGBColorSpace c a) => Film (RGBFilm c p a) p a where
  addSample (P (V2 x y)) radiance λ _ weight RGBFilm{..} = do
    let FilmCommon{..} = _rgbFilmCommon
    let rgb = sensorResponse radiance λ _fcSensor
    let m = maximum rgb
    let rgb'
          | m > _rgbFilmMaxComponentValue = rgb ^* (_rgbFilmMaxComponentValue / m)
          | otherwise = rgb
    let rgbWeighted = rgb' ^* weight
    let RGBPixel{..} = _rgbFilmPixels V.! x V.! y
    modifyIORef rgbSum (+ rgbWeighted)
    modifyIORef weightSum (+ weight)
  usesVisibleSurface _ = False
  addSplat pos radiance λ f@RGBFilm{..} = do
    let FilmCommon{..} = _rgbFilmCommon
    let rgb = sensorResponse radiance λ _fcSensor
    let m = maximum rgb
    let rgb'
          | m > _rgbFilmMaxComponentValue = rgb ^* (_rgbFilmMaxComponentValue / m)
          | otherwise = rgb
    let posDiscrete = pos .+^ 0.5
    let radius = filterRadius _fcFilter
    let splatBounds =
          Bounds.intersect (pixelBounds f) $
            Bounds.union
              (Bounds.singularity $ floor <$> posDiscrete .-^ radius)
              (Bounds.singularity $ (floor <$> posDiscrete .+^ radius) .+^ 1)
    for_ (Bounds.pointsWithin splatBounds) \i -> do
      let weight = evalFilter _fcFilter $ P $ pos .-. (fromIntegral <$> i) ^+^ 0.5
      unless (weight == 0) do
        let P (V2 x y) = i
        let RGBPixel{..} = _rgbFilmPixels V.! x V.! y
        atomicModifyIORef splatSum $ (,()) . (+ rgb')
  getPixelRGB (P (V2 x y)) splatScale RGBFilm{..} = do
    let RGBPixel{..} = _rgbFilmPixels V.! x V.! y
    rgbSamples <- readIORef rgbSum
    weight <- readIORef weightSum
    splatRgb <- readIORef splatSum
    let rgbSamplesWeighted
          | weight /= 0 = rgbSamples ^/ weight
          | otherwise = rgbSamples
    let splatRgbScaled = (splatScale *^ splatRgb) ^/ _rgbFilmFilterIntegral
    pure $ coerce _rgbFilmOutputRBGFromSensorRGB !* (splatRgbScaled ^+^ rgbSamplesWeighted)
  toOutputRGB radiance λ RGBFilm{..} =
    coerce _rgbFilmOutputRBGFromSensorRGB !* sensorResponse radiance λ _fcSensor
    where
      FilmCommon{..} = _rgbFilmCommon
