{-# LANGUAGE QuantifiedConstraints #-}

module Graphics.Color where

import Control.Lens (Each, Field1 (..), Field2 (..), Field3 (..), Lens', coerced, (^.))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Data.Coerce (coerce)
import Data.Distributive (Distributive)
import Data.Foldable1 (Foldable1)
import Data.Functor.Alt (Apply)
import Data.Functor.Bind (Bind)
import Data.Functor.Classes
import Data.Ix (Ix)
import Data.Reflection (Reifies)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat)
import Linear
import Linear.Affine (Affine, Point (..))
import Linear.V (Finite (..))
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse (Reverse, auto, jacobian')
import Numeric.FMA (FMA)
import Numeric.Natural (Natural)
import Physics.Spectrum
import Physics.Spectrum.Class
import Physics.Spectrum.Interpolated (InterpolatedSpectrum)
import Physics.Spectrum.Mul (MulSpectrum (..))
import Physics.Spectrum.RGB
import Physics.Spectrum.Sampled (
  HasSpectrumSamples (SampledSpectrum, meanSample, safeDiv),
  SampledWavelengths,
  sampledWavelengthsPdf,
 )
import System.Random (Random)

newtype XYZ a = XYZ {unXYZ :: V3 a}
  deriving stock (Show, Read, Eq, Ord, Generic, Traversable, Generic1)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadZip
    , Foldable
    , Eq1
    , Ord1
    , Additive
    , Affine
    , Metric
    , Trace
    , Finite
    , Apply
    , Bind
    , Foldable1
    , Semigroup
    , Monoid
    , Bounded
    , Num
    , Fractional
    , Floating
    , Ix
    , Epsilon
    , Random
    )

instance Each (XYZ a) (XYZ b) a b

instance Field1 (XYZ a) (XYZ a) a a where
  _1 = coerced @_ @_ @(V3 a) @(V3 a) . _1

instance Field2 (XYZ a) (XYZ a) a a where
  _2 = coerced @_ @_ @(V3 a) @(V3 a) . _2

instance Field3 (XYZ a) (XYZ a) a a where
  _3 = coerced @_ @_ @(V3 a) @(V3 a) . _3

instance R1 XYZ where
  _x = _1

instance R2 XYZ where
  _y = _2
  _xy = coerced @_ @_ @(V3 _) @(V3 _) . _xy

instance R3 XYZ where
  _z = _3
  _xyz = coerced @_ @_ @(V3 _) @(V3 _) . _xyz

xyFromXYZ :: (Fractional a) => XYZ a -> Point V2 a
xyFromXYZ xyz = P $ (xyz ^. _xy) ^/ sum xyz

xyYToXYZ :: (Fractional a, Eq a) => Point V2 a -> a -> XYZ a
xyYToXYZ (P (V2 x y)) y'
  | y == 0 = 0
  | otherwise = XYZ $ V3 (x * y' / y) y' $ (1 - x - y) * y' / y

xyToXYZ :: (Fractional a, Eq a) => Point V2 a -> XYZ a
xyToXYZ xy = xyYToXYZ xy 1

spectrumToXYZ :: (RealFloat a, Spectrum s a) => s -> XYZ a
spectrumToXYZ s =
  (integrateVisible . MulSpectrum s <$> XYZ (V3 cieX cieY cieZ)) ^/ cieYIntegral

sampledSpectrumToXYZ
  :: (RealFloat a, HasSpectrumSamples n a, Num (SampledSpectrum n a))
  => SampledWavelengths n a
  -> SampledSpectrum n a
  -> XYZ a
sampledSpectrumToXYZ λs s =
  XYZ
    ( V3
        (meanSample $ safeDiv (x * s) pdf)
        (meanSample $ safeDiv (y * s) pdf)
        (meanSample $ safeDiv (z * s) pdf)
    )
    ^/ cieYIntegral
  where
    pdf = sampledWavelengthsPdf λs
    x = sampleSpectrum λs cieX
    y = sampleSpectrum λs cieY
    z = sampleSpectrum λs cieZ

newtype RGB a = RGB {unRGB :: V3 a}
  deriving stock (Show, Read, Eq, Ord, Generic, Traversable, Generic1)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadZip
    , Foldable
    , Eq1
    , Ord1
    , Additive
    , Affine
    , Metric
    , Trace
    , Finite
    , Apply
    , Bind
    , Foldable1
    , Semigroup
    , Monoid
    , Bounded
    , Num
    , Fractional
    , Floating
    , Ix
    , Epsilon
    , Random
    )

instance Each (RGB a) (RGB b) a b

instance Field1 (RGB a) (RGB a) a a where
  _1 = coerced @_ @_ @(V3 a) @(V3 a) . _1

instance Field2 (RGB a) (RGB a) a a where
  _2 = coerced @_ @_ @(V3 a) @(V3 a) . _2

instance Field3 (RGB a) (RGB a) a a where
  _3 = coerced @_ @_ @(V3 a) @(V3 a) . _3

instance R1 RGB where
  _x = _1

instance R2 RGB where
  _y = _2
  _xy = coerced @_ @_ @(V3 _) @(V3 _) . _xy

instance R3 RGB where
  _z = _3
  _xyz = coerced @_ @_ @(V3 _) @(V3 _) . _xyz

_r :: Lens' (RGB a) a
_r = _x

_g :: Lens' (RGB a) a
_g = _y

_b :: Lens' (RGB a) a
_b = _z

class (RealFloat a) => RGBColorSpace c a where
  mRgbFromXYZ :: c -> RGB (XYZ a)
  mRgbToXYZ :: c -> XYZ (RGB a)
  rgbPrimaries :: c -> RGB (Point V2 a)
  whitePoint :: c -> InterpolatedSpectrum a
  whitePointChroma :: c -> Point V2 a

colorSpaceLuminanceVector :: forall c a. (RGBColorSpace c a) => c -> RGB a
colorSpaceLuminanceVector c = mRgbToXYZ c ^. _y

mRgbToRGB :: (RGBColorSpace c a, RGBColorSpace d a) => c -> d -> RGB (RGB a)
mRgbToRGB c d = mRgbFromXYZ d !*! mRgbToXYZ c

rgbToRGB :: (RGBColorSpace c a, RGBColorSpace d a) => c -> d -> RGB a -> RGB a
rgbToRGB c d = (mRgbToRGB c d !*)

rgbToXYZ :: (RGBColorSpace c a) => c -> RGB a -> XYZ a
rgbToXYZ c = (mRgbToXYZ c !*)

rgbFromXYZ :: (RGBColorSpace c a) => c -> XYZ a -> RGB a
rgbFromXYZ c = (mRgbFromXYZ c !*)

spectrumToRGB :: (RGBColorSpace c a, Spectrum s a) => c -> s -> RGB a
spectrumToRGB c = rgbFromXYZ c . spectrumToXYZ

sampledSpectrumToRGB
  :: (RGBColorSpace c a, HasSpectrumSamples n a, Num (SampledSpectrum n a))
  => c
  -> SampledWavelengths n a
  -> SampledSpectrum n a
  -> RGB a
sampledSpectrumToRGB c = fmap (rgbFromXYZ c) . sampledSpectrumToXYZ

rgbAlbedo :: forall c a. (RGBColorSpace c a, FMA a) => c -> RGB a -> RGBSpectrum a
rgbAlbedo c rgb@(RGB (V3 r g b))
  | r == g && g == b = rgbAlbedoSpectrum 0 0 $ (r - 0.5) / sqrt (r * (r - 1))
  | otherwise = albedoSpectrum rgb
  where
    roundtrip :: forall s. (Reifies s Tape) => RGB a -> V3 (Reverse s a) -> RGB (Reverse s a)
    roundtrip rgb' (V3 c0' c1' c2') =
      (auto <$> rgb') - spectrumToRGB c (MulSpectrum (whitePoint c) $ rgbAlbedoSpectrum c0' c1' c2')

    albedoSpectrum rgb' =
      let V3 c0 c1 c2 = gaussNewton (roundtrip rgb') 1.0e-6 (V3 0 0 0) 50
       in rgbAlbedoSpectrum c0 c1 c2

rgbLight :: forall c a. (RGBColorSpace c a, FMA a) => c -> RGB a -> RGBSpectrum a
rgbLight c = rgbIlluminant c $ whitePoint c

rgbIlluminant :: forall c a. (RGBColorSpace c a, FMA a) => c -> InterpolatedSpectrum a -> RGB a -> RGBSpectrum a
rgbIlluminant c illuminant rgb = (rgbAlbedo c rgb){_rgbIlluminant = Just illuminant}

gaussNewton
  :: ( Metric f
     , Traversable f
     , Distributive f
     , Finite f
     , KnownNat (Size f)
     , Foldable g
     , Fractional (f a)
     , RealFloat a
     , Metric g
     )
  => (forall s. (Reifies s Tape, Typeable s) => f (Reverse s a) -> g (Reverse s a))
  -- ^ The function to compute the residual
  -> a
  -- ^ The absolute error tolerance
  -> f a
  -- ^ The initial guess of the parameters
  -> Natural
  -- ^ Maximum iterations
  -> f a
gaussNewton r tol = go
  where
    go β 0 = β
    go β it
      | any isNaN β' = β
      | err < tol = β'
      | otherwise = go β' $ pred it
      where
        rβXjβ = jacobian' r β
        rβ = fst <$> rβXjβ
        jβ = snd <$> rβXjβ
        jβT = transpose jβ
        jβInv = luInvFinite (jβT !*! jβ) !*! jβT
        err = quadrance rβ
        d = jβInv !* rβ
        constrain x
          | maxX > 200 = (/ maxX) . (* 2000) <$> x
          | otherwise = x
          where
            maxX = maximum x
        β' = constrain $ β ^-^ d

type ColorSpaceData a = (RGB (XYZ a), XYZ (RGB a), Point V2 a)

mkColorSpace
  :: (RealFloat a)
  => RGB (Point V2 a)
  -> InterpolatedSpectrum a
  -> ColorSpaceData a
mkColorSpace primaries wp =
  (coerce $ inv33 m, coerce m, xyFromXYZ wpXYZ)
  where
    wpXYZ = spectrumToXYZ wp
    V3 r g b = coerce $ xyToXYZ <$> primaries
    rgb = transpose $ V3 r g b
    V3 c0 c1 c2 = inv33 rgb !* unXYZ wpXYZ
    m =
      rgb
        !*! V3
          (V3 c0 0 0)
          (V3 0 c1 0)
          (V3 0 0 c2)

data SRGB = SRGB
  deriving (Show, Read, Eq, Ord)

instance (Reifies s Tape, RGBColorSpace c a) => RGBColorSpace c (Reverse s a) where
  mRgbFromXYZ = (fmap . fmap) auto . mRgbFromXYZ
  mRgbToXYZ = (fmap . fmap) auto . mRgbToXYZ
  rgbPrimaries = (fmap . fmap) auto . rgbPrimaries
  whitePoint = fmap (realToFrac @a) . whitePoint
  whitePointChroma = fmap auto . whitePointChroma

instance RGBColorSpace SRGB Float where
  mRgbFromXYZ _ = sRGBData ^. _1
  mRgbToXYZ _ = sRGBData ^. _2
  rgbPrimaries _ = sRGBPrimaries
  whitePoint _ = cieD65
  whitePointChroma _ = sRGBData ^. _3

instance RGBColorSpace SRGB Double where
  mRgbFromXYZ _ = sRGBData ^. _1
  mRgbToXYZ _ = sRGBData ^. _2
  rgbPrimaries _ = sRGBPrimaries
  whitePoint _ = cieD65
  whitePointChroma _ = sRGBData ^. _3

data DCI_P3 = DCI_P3
  deriving (Show, Read, Eq, Ord)

instance RGBColorSpace DCI_P3 Float where
  mRgbFromXYZ _ = dci_P3Data ^. _1
  mRgbToXYZ _ = dci_P3Data ^. _2
  rgbPrimaries _ = dci_P3Primaries
  whitePoint _ = cieD65
  whitePointChroma _ = dci_P3Data ^. _3

data Rec2020 = Rec2020
  deriving (Show, Read, Eq, Ord)

instance RGBColorSpace Rec2020 Float where
  mRgbFromXYZ _ = rec2020Data ^. _1
  mRgbToXYZ _ = rec2020Data ^. _2
  rgbPrimaries _ = rec2020Primaries
  whitePoint _ = cieD65
  whitePointChroma _ = rec2020Data ^. _3

data ACES2065_1 = ACES2065_1
  deriving (Show, Read, Eq, Ord)

instance RGBColorSpace ACES2065_1 Float where
  mRgbFromXYZ _ = aces2065_1Data ^. _1
  mRgbToXYZ _ = aces2065_1Data ^. _2
  rgbPrimaries _ = aces2065_1Primaries
  whitePoint _ = acesD60
  whitePointChroma _ = aces2065_1Data ^. _3

sRGBPrimaries :: (RealFloat a) => RGB (Point V2 a)
sRGBPrimaries =
  RGB $ V3 (P $ V2 0.64 0.33) (P $ V2 0.3 0.6) (P $ V2 0.15 0.06)

{-# SPECIALIZE sRGBData :: ColorSpaceData Float #-}
{-# SPECIALIZE sRGBData :: ColorSpaceData Double #-}
sRGBData
  :: (RealFloat a, Enum a)
  => ColorSpaceData a
sRGBData = mkColorSpace sRGBPrimaries cieD65

dci_P3Primaries :: (RealFloat a) => RGB (Point V2 a)
dci_P3Primaries =
  RGB $ V3 (P $ V2 0.68 0.32) (P $ V2 0.256 0.69) (P $ V2 0.15 0.06)

{-# SPECIALIZE dci_P3Data :: ColorSpaceData Float #-}
{-# SPECIALIZE dci_P3Data :: ColorSpaceData Double #-}
dci_P3Data
  :: (RealFloat a, Enum a)
  => ColorSpaceData a
dci_P3Data = mkColorSpace dci_P3Primaries cieD65

rec2020Primaries :: (RealFloat a) => RGB (Point V2 a)
rec2020Primaries =
  RGB $ V3 (P $ V2 0.708 0.292) (P $ V2 0.17 0.797) (P $ V2 0.131 0.046)

{-# SPECIALIZE rec2020Data :: ColorSpaceData Float #-}
{-# SPECIALIZE rec2020Data :: ColorSpaceData Double #-}
rec2020Data
  :: (RealFloat a, Enum a)
  => ColorSpaceData a
rec2020Data = mkColorSpace rec2020Primaries cieD65

aces2065_1Primaries :: (RealFloat a) => RGB (Point V2 a)
aces2065_1Primaries =
  RGB $ V3 (P $ V2 0.7347 0.2653) (P $ V2 0 1) (P $ V2 0.0001 (-0.077))

{-# SPECIALIZE aces2065_1Data :: ColorSpaceData Float #-}
{-# SPECIALIZE aces2065_1Data :: ColorSpaceData Double #-}
aces2065_1Data
  :: (RealFloat a, Enum a)
  => ColorSpaceData a
aces2065_1Data = mkColorSpace aces2065_1Primaries acesD60
