{-# LANGUAGE MultiWayIf #-}

module Main where

import Codec.Picture (PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (generateImage)
import Data.Function (on)
import Data.Ord (clamp)
import Data.Word (Word8)
import Debug.Trace (traceShow, traceShowId)
import Geometry.Bounds (Bounds (..))
import Graphics.Color (RGB (..), SRGB (..), rgbLight, sampleRGB, spectrumToRGB)
import Linear
import Linear.Affine (Point (..))
import Physics.Spectrum (
  Spectrum (..),
  addSpectrum,
  blackbodySpectrum,
  cieY,
  constSpectrum,
  evalSpectrum,
  interpolatedSpectrum,
  mulSpectrum,
  sampleWavelengthsVisible,
  upperBound,
  visibleBounds,
 )

main :: IO ()
main = do
  let spec = rgbLight SRGB $ RGB $ V3 1 0 1
  writePng "output.png" $ generateImage (render spec) xRes yRes

xRes :: Int
xRes = 400

yRes :: Int
yRes = 300

render :: Spectrum Float -> Int -> Int -> PixelRGB8
render spectrum = \x y -> do
  let l = remapX x
  let y' = remapY y
  let a = evalSpectrum l spectrum
  let t = clamp (0, 1) $ abs ((a - y') * fromIntegral yRes) - 2
  let near i j = abs (j - i) < 0.5
  let rgb'' =
        if
          | near l lMin || near l lMax -> 1 - bg
          | on near (* fromIntegral yRes) y' yMax -> RGB $ V3 1 0 0
          | otherwise -> lerp t bg rgb'
  rgbToColor rgb''
  where
    bg
      | norm rgb <= 1 = 1
      | otherwise = 0
    rgb' = traceShowId $ sampleRGB SRGB (sampleWavelengthsVisible 0 471) spectrum
    rgb = traceShowId $ spectrumToRGB SRGB spectrum
    Bounds (P (V1 lMin)) (P (V1 lMax)) = visibleBounds spectrum
    yMax = traceShow (rgbToColor rgb) $ upperBound spectrum

remapY :: Int -> Float
remapY = subtract 0.1 . (* 1.25) . (1 -) . (/ fromIntegral yRes) . fromIntegral

remapX :: Int -> Float
remapX = (+ 360) . (* 471) . (/ fromIntegral xRes) . fromIntegral

rgbToColor :: RGB Float -> PixelRGB8
rgbToColor (RGB (V3 r g b)) = PixelRGB8 (gamma r) (gamma g) (gamma b)

gamma :: Float -> Word8
gamma = round . (* 255) . (** (1 / 2.2)) . clamp (0, 1)

polySpectrum :: Spectrum Float
polySpectrum = SigmoidQuadraticSpectrum (-1 / 32000) (119 / 3200) (-13521 / 1280)

bbSpectrum :: Spectrum Float
bbSpectrum = blackbodySpectrum 2500

addS :: Spectrum Float
addS = mulSpectrum (addSpectrum polySpectrum bbSpectrum) (constSpectrum 0.5)

mulS :: Spectrum Float
mulS = mulSpectrum polySpectrum bbSpectrum

interpolateS :: Spectrum Float
interpolateS = interpolatedSpectrum [(400, 0.25), (500, 1), (600, 0), (700, 0.8), (800, 0.5)]

sampledS :: Spectrum Float
sampledS = cieY
