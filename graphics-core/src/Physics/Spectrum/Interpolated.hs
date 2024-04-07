module Physics.Spectrum.Interpolated (
  InterpolatedSpectrum,
  interpolatedSpectrum,
) where

import Control.Lens (Bifunctor (..))
import Data.Function (on)
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Physics.Spectrum.Class

newtype InterpolatedSpectrum a = InterpolatedSpectrum
  { _isSamples :: Vector ((a, a), (a, a))
  }
  deriving (Show, Eq, Ord, Functor)

interpolatedSpectrum :: (Ord a) => [(a, a)] -> InterpolatedSpectrum a
interpolatedSpectrum =
  InterpolatedSpectrum . V.fromList . intervals . fmap head . groupBy (on (==) fst) . sortOn fst

intervals :: [a] -> [(a, a)]
intervals [] = []
intervals [_] = []
intervals (a : b : as) = (a, b) : intervals (b : as)

instance (RealFrac a) => Spectrum (InterpolatedSpectrum a) a where
  {-# SPECIALIZE instance Spectrum (InterpolatedSpectrum Float) Float #-}
  {-# SPECIALIZE instance Spectrum (InterpolatedSpectrum Double) Double #-}
  evalSpectrum λ InterpolatedSpectrum{..} = interpolateInterval λ _isSamples
  {-# INLINE evalSpectrum #-}
  scaleSpectrum s InterpolatedSpectrum{..} =
    InterpolatedSpectrum $ fmap (bimap (fmap (s *)) (fmap (s *))) _isSamples
  {-# INLINE scaleSpectrum #-}
  maxValue InterpolatedSpectrum{..} = maximum $ uncurry max . bimap snd snd <$> _isSamples
  {-# INLINE maxValue #-}
  minValue InterpolatedSpectrum{..} = minimum $ uncurry min . bimap snd snd <$> _isSamples
  {-# INLINE minValue #-}

interpolateInterval :: (RealFrac a) => a -> V.Vector ((a, a), (a, a)) -> a
interpolateInterval λ v
  | V.null v = 0
  | otherwise =
      let (l, ((λ0, a0), (λ1, a1)), r) = unsafeBisect λ v
       in case compare λ λ0 of
            LT -> interpolateInterval λ l
            EQ -> a0
            GT -> case compare λ λ1 of
              LT -> lerpScalar ((λ - λ0) / (λ1 - λ0)) a0 a1
              EQ -> a1
              GT -> interpolateInterval λ r

unsafeBisect
  :: (RealFrac a)
  => a
  -> V.Vector ((a, a), (a, a))
  -> (V.Vector ((a, a), (a, a)), ((a, a), (a, a)), V.Vector ((a, a), (a, a)))
unsafeBisect λ v = (l, V.unsafeHead r, V.unsafeTail r)
  where
    (l, r) = V.splitAt i v
    i = min (V.length v - 1) $ floor $ (λ - λ0) / (λn - λ0)
    λ0 = fst $ fst $ V.unsafeHead v
    λn = fst $ snd $ V.unsafeLast v
{-# INLINE unsafeBisect #-}

lerpScalar :: (Eq a, Num a) => a -> a -> a -> a
lerpScalar 0 a _ = a
lerpScalar 1 _ b = b
lerpScalar t a b = (1 - t) * a + t * b
