module Physics.Spectrum.Histogram (
  HistogramSpectrum,
  histogramSpectrum,
) where

import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Physics.Spectrum.Class

data HistogramSpectrum a = HistogramSpectrum
  { _hsλMin :: Int
  , _hsSamples :: Vector a
  , _hsMaxValue :: a
  , _hsMinValue :: a
  }
  deriving (Show, Eq, Ord, Functor)

histogramSpectrum :: (Ord a, Num a) => Int -> Vector a -> HistogramSpectrum a
histogramSpectrum λmin samples
  | V.null samples' = HistogramSpectrum λmin' samples' 0 0
  | otherwise = HistogramSpectrum λmin' samples' (maximum samples') (minimum samples')
  where
    invisiblePrefix = max 0 $ 360 - λmin
    visibleSamples = V.drop invisiblePrefix samples
    firstNonZero = fromMaybe (V.length visibleSamples) $ V.findIndex (> 0) visibleSamples
    samplesTrimmed = V.drop firstNonZero visibleSamples
    λmin' = λmin + (firstNonZero + invisiblePrefix)
    trimmedSpan = V.length samplesTrimmed
    invisibleSuffix = max 0 $ λmin' + trimmedSpan - 830 - 1
    samples' = V.take (trimmedSpan - invisibleSuffix) samplesTrimmed

instance (RealFrac a) => Spectrum (HistogramSpectrum a) a where
  {-# SPECIALIZE instance Spectrum (HistogramSpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (HistogramSpectrum Double) Double #-}
  evalSpectrum λ HistogramSpectrum{..} = case round λ - _hsλMin of
    offset
      | offset < 0 || offset >= V.length _hsSamples -> 0
      | otherwise -> _hsSamples V.! offset
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s HistogramSpectrum{..} =
    histogramSpectrum _hsλMin $ (s *) <$> _hsSamples
  {-# INLINE scaleSpectrum #-}
  maxValue HistogramSpectrum{..} = _hsMaxValue
  {-# INLINE maxValue #-}
  minValue HistogramSpectrum{..} = _hsMinValue
  {-# INLINE minValue #-}
