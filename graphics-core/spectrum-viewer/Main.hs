{-# LANGUAGE MultiWayIf #-}

module Main where

import Codec.Picture (PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (generateImage)
import Data.Function (on)
import Data.Ord (clamp)
import Data.Word (Word8)
import Geometry.Bounds (Bounds (..))
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
  spectrumToXYZ,
  upperBound,
  visibleBounds,
 )

main :: IO ()
main = writePng "output.png" $ generateImage (render polySpectrum) xRes yRes

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
  let rgb' =
        if
          | near l lMin || near l lMax -> 1 - bg
          | on near (* fromIntegral yRes) y' yMax -> V3 1 0 0
          | otherwise -> lerp t bg rgb
  v3ToColor rgb'
  where
    bg
      | norm rgb <= 1 = 1
      | otherwise = 0
    rgb = xyzToSRgbLinear $ spectrumToXYZ spectrum
    Bounds (P (V1 lMin)) (P (V1 lMax)) = visibleBounds spectrum
    yMax = upperBound spectrum

remapY :: Int -> Float
remapY = subtract 0.1 . (* 1.25) . (1 -) . (/ fromIntegral yRes) . fromIntegral

remapX :: Int -> Float
remapX = (+ 360) . (* 471) . (/ fromIntegral xRes) . fromIntegral

xyzToSRgbLinear :: V3 Float -> V3 Float
xyzToSRgbLinear = (mInv !*)

mInv :: V3 (V3 Float)
mInv =
  V3
    (V3 3.2404542 (-1.5371385) (-0.4985314))
    (V3 (-0.9692660) 1.8760108 0.0415560)
    (V3 0.0556434 (-0.2040259) 1.0572252)

v3ToColor :: V3 Float -> PixelRGB8
v3ToColor v = PixelRGB8 (gamma r) (gamma g) (gamma b)
  where
    V3 r g b = v

gamma :: Float -> Word8
gamma f = floor $ clamp (0, 1) v * 255
  where
    v
      | f <= 0.0031308 = 12.92 * f
      | otherwise = 1.05 * (f ** (1 / 2.4)) - 0.005

polySpectrum :: Spectrum Float
polySpectrum = SigmoidQuadraticSpectrum 0 (-1 / 40) 10

bbSpectrum :: Spectrum Float
bbSpectrum = blackbodySpectrum 6500

addS :: Spectrum Float
addS = mulSpectrum (addSpectrum polySpectrum bbSpectrum) (constSpectrum 0.5)

mulS :: Spectrum Float
mulS = mulSpectrum polySpectrum bbSpectrum

interpolateS :: Spectrum Float
interpolateS = interpolatedSpectrum [(400, 0.25), (500, 1), (600, 0), (700, 0.8), (800, 0.5)]

sampledS :: Spectrum Float
sampledS = cieY
