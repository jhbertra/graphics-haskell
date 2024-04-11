module Graphics.Rendering.Film where

import GHC.TypeLits (KnownNat)
import Geometry.Bounds (Bounds2, Bounds2i)
import qualified Geometry.Bounds as Bounds
import Geometry.Normal
import Graphics.Color
import Graphics.Rendering.Filter
import Graphics.Rendering.Sensor
import Linear
import Linear.Affine
import Numeric.FMA (FMA)
import Physics.Spectrum.Sampled

class (Filter p a, RealFloat a, FMA a) => Film f p a | f -> p a where
  addSample
    :: (KnownNat n, HasSpectrumSamples n a)
    => Point V2 Int
    -> SampledSpectrum n a
    -> SampledWavelengths n a
    -> Maybe (VisibleSurface n a)
    -> a
    -> f
    -> IO ()
  sampleBounds :: f -> Bounds2 a
  default sampleBounds :: (HasFilmCommon f p a) => f -> Bounds2 a
  sampleBounds f =
    Bounds.intersect
      (Bounds.singularity $ fmap fromIntegral pbMin .-^ radius .+^ 0.5)
      (Bounds.singularity $ fmap fromIntegral pbMax .+^ radius .-^ 0.5)
    where
      Bounds.Bounds pbMin pbMax = pixelBounds f
      FilmCommon{..} = getFilmCommon f
      radius = filterRadius _fcFilter
  usesVisibleSurface :: f -> Bool
  addSplat
    :: (KnownNat n, HasSpectrumSamples n a)
    => Point V2 a
    -> SampledSpectrum n a
    -> SampledWavelengths n a
    -> f
    -> IO ()
  sampleWavelengths
    :: (KnownNat n, HasSpectrumSamples n a)
    => f
    -> a
    -> SampledWavelengths n a
  sampleWavelengths _ = sampleVisibleWavelengths
  resolution :: f -> Point V2 Int
  default resolution :: (HasFilmCommon f p a) => f -> Point V2 Int
  resolution = _fcResolution . getFilmCommon
  pixelBounds :: f -> Bounds2i
  default pixelBounds :: (HasFilmCommon f p a) => f -> Bounds2i
  pixelBounds = _fcPixelBounds . getFilmCommon
  filmDiagonal :: f -> a
  default filmDiagonal :: (HasFilmCommon f p a) => f -> a
  filmDiagonal = _fcDiagonal . getFilmCommon
  toOutputRGB
    :: (KnownNat n, HasSpectrumSamples n a)
    => SampledSpectrum n a
    -> SampledWavelengths n a
    -> f
    -> RGB a
  getPixelRGB :: Point V2 Int -> a -> f -> IO (RGB a)
  getSensor :: f -> Sensor a
  default getSensor :: (HasFilmCommon f p a) => f -> Sensor a
  getSensor = _fcSensor . getFilmCommon
  getFilter :: f -> p
  default getFilter :: (HasFilmCommon f p a) => f -> p
  getFilter = _fcFilter . getFilmCommon

data VisibleSurface n a = VisibleSurface
  { _vsPos :: Point V3 a
  , _vsNormal :: Normal V3 a
  , _vsShadingNormal :: Normal V3 a
  , _vsTime :: a
  , _vsParametricCoords :: Point V2 a
  , _vsDpdx :: V3 a
  , _vsDpdy :: V3 a
  , _vsAlbedo :: SampledSpectrum n a
  }

deriving instance (Eq a, Eq (SampledSpectrum n a)) => Eq (VisibleSurface n a)
deriving instance (Ord a, Ord (SampledSpectrum n a)) => Ord (VisibleSurface n a)
deriving instance (Show a, Show (SampledSpectrum n a)) => Show (VisibleSurface n a)

data FilmCommon p a = FilmCommon
  { _fcResolution :: Point V2 Int
  , _fcPixelBounds :: Bounds2i
  , _fcDiagonal :: a
  , _fcSensor :: Sensor a
  , _fcFilter :: p
  }

class HasFilmCommon f p a | f -> p a where
  getFilmCommon :: f -> FilmCommon p a
