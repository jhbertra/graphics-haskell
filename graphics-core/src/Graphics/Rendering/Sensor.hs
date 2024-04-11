module Graphics.Rendering.Sensor (
  Sensor,
  sensorXYZWhiteBalance,
  sensorXYZ,
  sensorResponse,
) where

import Graphics.Color
import Linear
import Linear.Affine
import Physics.Spectrum (cieX, cieY, cieZ)
import Physics.Spectrum.Class (Spectrum (sampleSpectrum))
import Physics.Spectrum.Histogram (HistogramSpectrum)
import Physics.Spectrum.Sampled (HasSpectrumSamples (..), SampledSpectrum, SampledWavelengths, sampledWavelengthsPdf)

data Sensor a = Sensor
  { _sensorR :: HistogramSpectrum a
  , _sensorG :: HistogramSpectrum a
  , _sensorB :: HistogramSpectrum a
  , _sensorImagingRatio :: a
  , _sensorXYZFromRGB :: M33 a
  }
  deriving (Show, Eq, Ord)

sensorXYZ :: (Fractional a, Ord a) => a -> Sensor a
sensorXYZ imagingRatio =
  Sensor
    { _sensorR = cieX
    , _sensorG = cieY
    , _sensorB = cieZ
    , _sensorImagingRatio = imagingRatio
    , _sensorXYZFromRGB = identity
    }

sensorXYZWhiteBalance :: (RGBColorSpace c a, Spectrum s a) => c -> s -> a -> Sensor a
sensorXYZWhiteBalance outputColorSpace sensorIlluminant imagingRatio =
  Sensor
    { _sensorR = cieX
    , _sensorG = cieY
    , _sensorB = cieZ
    , _sensorImagingRatio = imagingRatio
    , _sensorXYZFromRGB =
        whiteBalance
          (xyFromXYZ $ spectrumToXYZ sensorIlluminant)
          (whitePointChroma outputColorSpace)
    }

whiteBalance :: (Fractional a, Eq a) => Point V2 a -> Point V2 a -> M33 a
whiteBalance srcWhite targetWhite = xyzFromLMS !*! lmsCorrect !*! lmsFromXYZ
  where
    XYZ srcXYZ = xyToXYZ srcWhite
    XYZ targetXYZ = xyToXYZ targetWhite
    srcLMS = lmsFromXYZ !* srcXYZ
    targetLMS = lmsFromXYZ !* targetXYZ
    V3 x y z = targetLMS / srcLMS
    lmsCorrect =
      V3
        (V3 x 0 0)
        (V3 0 y 0)
        (V3 0 0 z)

lmsFromXYZ :: M33 a
lmsFromXYZ = _

xyzFromLMS :: M33 a
xyzFromLMS = _

sensorResponse
  :: (HasSpectrumSamples n a, Num (SampledSpectrum n a), RealFrac a)
  => SampledSpectrum n a
  -> SampledWavelengths n a
  -> Sensor a
  -> RGB a
sensorResponse radiance λ Sensor{..} =
  _sensorImagingRatio
    *^ RGB
      ( V3
          (meanSample (sampleSpectrum λ _sensorR * radianceWeighted))
          (meanSample (sampleSpectrum λ _sensorG * radianceWeighted))
          (meanSample (sampleSpectrum λ _sensorB * radianceWeighted))
      )
  where
    radianceWeighted = safeDiv radiance (sampledWavelengthsPdf λ)
