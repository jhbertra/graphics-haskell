module Physics.Spectrum where

import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import Geometry.Bounds (Bounds (Bounds), Bounds1)
import qualified Geometry.Bounds as Bounds
import Linear
import Linear.Affine (Point (..))

data Spectrum a where
  ConstSpectrum :: a -> Spectrum a
  SigmoidQuadraticSpectrum :: a -> a -> a -> Spectrum a
  SampledSpectrum :: Word16 -> V.Vector a -> Spectrum a
  InterpolatedSpectrum :: V.Vector ((a, a), (a, a)) -> Spectrum a
  BlackbodySpectrum :: Blackbody a -> Spectrum a
  AddSpectrum :: Spectrum a -> Spectrum a -> Spectrum a
  MulSpectrum :: Spectrum a -> Spectrum a -> Spectrum a

deriving instance (Eq a) => Eq (Spectrum a)
deriving instance (Show a) => Show (Spectrum a)

realToFracSpectrum :: (Real a, Fractional b) => Spectrum a -> Spectrum b
realToFracSpectrum = \case
  ConstSpectrum a -> ConstSpectrum $ realToFrac a
  SigmoidQuadraticSpectrum a b c -> SigmoidQuadraticSpectrum (realToFrac a) (realToFrac b) (realToFrac c)
  SampledSpectrum λMin samples -> SampledSpectrum λMin $ realToFrac <$> samples
  InterpolatedSpectrum points -> InterpolatedSpectrum $ bimap (bimap realToFrac realToFrac) (bimap realToFrac realToFrac) <$> points
  BlackbodySpectrum bb -> BlackbodySpectrum $ realToFrac <$> bb
  AddSpectrum a b -> AddSpectrum (realToFracSpectrum a) (realToFracSpectrum b)
  MulSpectrum a b -> MulSpectrum (realToFracSpectrum a) (realToFracSpectrum b)

constSpectrum :: (Ord a, Num a) => a -> Spectrum a
constSpectrum = ConstSpectrum . max 0
{-# INLINE constSpectrum #-}

sigmoid :: (RealFloat a) => a -> a
sigmoid a
  | isInfinite a = if a > 0 then 1 else 0
  | otherwise = 0.5 + a / (2 * sqrt (1 + a * a))
{-# INLINE sigmoid #-}

denselySampleSpectrum :: (RealFloat a) => Word8 -> Spectrum a -> Spectrum a
denselySampleSpectrum 0 _ = constSpectrum 0
denselySampleSpectrum _ s@(SampledSpectrum _ _) = s
denselySampleSpectrum subSamples s =
  sampledSpectrum minVisibleλ $ V.generate (fromIntegral visibleRange) \i ->
    let invNumSamples = 1 / fromIntegral subSamples
        samples = V.generate (fromIntegral subSamples) \j ->
          let t = fromIntegral j * invNumSamples
           in evalSpectrum (fromIntegral (fromIntegral minVisibleλ + i) + t) s
     in V.sum samples * invNumSamples

sampledSpectrum :: (Ord a, Num a) => Word16 -> V.Vector a -> Spectrum a
sampledSpectrum λmin samples
  | λmin' > maxVisibleλ || V.null samples' = mempty
  | otherwise = SampledSpectrum λmin' samples'
  where
    invisiblePrefix = max 0 $ fromIntegral minVisibleλ - fromIntegral λmin
    visibleSamples = V.drop invisiblePrefix samples
    firstNonZero = fromMaybe (V.length visibleSamples) $ V.findIndex (> 0) visibleSamples
    samplesTrimmed = V.drop firstNonZero visibleSamples
    λmin' = λmin + fromIntegral (firstNonZero + invisiblePrefix)
    trimmedSpan = V.length samplesTrimmed
    invisibleSuffix = max 0 $ fromIntegral λmin' + trimmedSpan - fromIntegral maxVisibleλ - 1
    samples' = V.take (trimmedSpan - invisibleSuffix) samplesTrimmed
{-# INLINE sampledSpectrum #-}

interpolatedSpectrum :: (Ord a) => [(a, a)] -> Spectrum a
interpolatedSpectrum =
  InterpolatedSpectrum . V.fromList . intervals . fmap head . groupBy (on (==) fst) . sortOn fst
{-# INLINE interpolatedSpectrum #-}

intervals :: [a] -> [(a, a)]
intervals [] = []
intervals [_] = []
intervals (a : b : as) = (a, b) : intervals (b : as)

normalizedInterpolatedSpectrum
  :: (RealFloat a, Bounded a)
  => [(a, a)]
  -> Spectrum a
normalizedInterpolatedSpectrum samples =
  scaleSpectrum (cieYIntegral / luminance) spec
  where
    spec = interpolatedSpectrum samples
    luminance = integrateVisible $ mulSpectrum cieY spec
{-# INLINE normalizedInterpolatedSpectrum #-}

blackbodySpectrum :: (Floating a, Ord a) => a -> Spectrum a
blackbodySpectrum = BlackbodySpectrum . mkBlackbody
{-# INLINE blackbodySpectrum #-}

mkBlackbody :: (Floating a, Ord a) => a -> Blackbody a
mkBlackbody bbTemperature = Blackbody{..}
  where
    λMax = 2.8977721e-3 / bbTemperature
    bbNormalizationFactor = 1 / blackbody bbTemperature (λMax * 1e9)
{-# INLINE mkBlackbody #-}

blackbody :: (Ord a, Floating a) => a -> a -> a
blackbody t
  | t <= 0 = const 0
  | otherwise = \λ -> do
      let λ' = λ * 1e-9
      (2 * 5.955206e-17) / ((λ' ** 5) * (exp (1.986443e-25 / (λ' * 1.3806488e-23 * t)) - 1))
{-# INLINE blackbody #-}

addSpectrum :: (Ord a, Num a) => Spectrum a -> Spectrum a -> Spectrum a
addSpectrum (ConstSpectrum 0) a = a
addSpectrum a (ConstSpectrum 0) = a
addSpectrum (ConstSpectrum a) (ConstSpectrum b) = constSpectrum $ a + b
addSpectrum a b = AddSpectrum a b
{-# INLINE addSpectrum #-}

scaleSpectrum :: (Ord a, Num a) => a -> Spectrum a -> Spectrum a
scaleSpectrum a s
  | a <= 0 = mempty
  | a == 1 = s
  | otherwise = case s of
      ConstSpectrum b -> constSpectrum $ a * b
      SampledSpectrum λ samples -> SampledSpectrum λ $ (a *) `V.map` samples
      InterpolatedSpectrum points -> InterpolatedSpectrum $ scaleIntervals a points
      AddSpectrum s0 s1 -> AddSpectrum (scaleSpectrum a s0) (scaleSpectrum a s1)
      MulSpectrum (ConstSpectrum b) (ConstSpectrum c) -> ConstSpectrum $ a * b * c
      MulSpectrum (ConstSpectrum b) (SampledSpectrum λ samples) ->
        SampledSpectrum λ $ (a * b *) `V.map` samples
      MulSpectrum (SampledSpectrum λ samples) (ConstSpectrum b) ->
        SampledSpectrum λ $ (a * b *) `V.map` samples
      MulSpectrum (ConstSpectrum b) (InterpolatedSpectrum points) ->
        InterpolatedSpectrum $ scaleIntervals (a * b) points
      MulSpectrum (InterpolatedSpectrum points) (ConstSpectrum b) ->
        InterpolatedSpectrum $ scaleIntervals (a * b) points
      MulSpectrum s' (SampledSpectrum λ samples) ->
        MulSpectrum s' $ SampledSpectrum λ $ (a *) `V.map` samples
      MulSpectrum (SampledSpectrum λ samples) s' ->
        MulSpectrum (SampledSpectrum λ $ (a *) `V.map` samples) s'
      MulSpectrum s' (InterpolatedSpectrum points) ->
        MulSpectrum s' $ InterpolatedSpectrum $ scaleIntervals a points
      MulSpectrum (InterpolatedSpectrum points) s' ->
        MulSpectrum (InterpolatedSpectrum $ scaleIntervals a points) s'
      MulSpectrum (ConstSpectrum b) s' -> MulSpectrum (ConstSpectrum $ a * b) s'
      MulSpectrum s' (ConstSpectrum b) -> MulSpectrum s' (ConstSpectrum $ a * b)
      _ -> MulSpectrum (ConstSpectrum a) s

scaleIntervals :: (Num a) => a -> V.Vector ((a, a), (a, a)) -> V.Vector ((a, a), (a, a))
scaleIntervals s = fmap (bimap (fmap (s *)) (fmap (s *)))

mulSpectrum :: (Ord a, Num a) => Spectrum a -> Spectrum a -> Spectrum a
mulSpectrum (ConstSpectrum 0) _ = mempty
mulSpectrum _ (ConstSpectrum 0) = mempty
mulSpectrum (ConstSpectrum 1) a = a
mulSpectrum a (ConstSpectrum 1) = a
mulSpectrum (ConstSpectrum a) (ConstSpectrum b) = constSpectrum $ a * b
mulSpectrum a b = MulSpectrum a b
{-# INLINE mulSpectrum #-}

instance (Num a, Ord a) => Semigroup (Spectrum a) where
  (<>) = addSpectrum
  {-# INLINE (<>) #-}

instance (Num a, Ord a) => Monoid (Spectrum a) where
  mempty = ConstSpectrum 0
  {-# INLINE mempty #-}

data Blackbody a = Blackbody
  { bbTemperature :: a
  , bbNormalizationFactor :: a
  }
  deriving (Eq, Ord, Show, Functor)

-- {-# SPECIALIZE maxVisibleBounds :: Bounds1 Float #-}
-- {-# SPECIALIZE maxVisibleBounds :: Bounds1 Double #-}
maxVisibleBounds :: (Num a) => Bounds1 a
maxVisibleBounds = on Bounds (P . V1) (fromIntegral minVisibleλ) (fromIntegral maxVisibleλ)

visibleRange :: Word16
visibleRange = maxVisibleλ - minVisibleλ + 1

maxVisibleλ :: Word16
maxVisibleλ = 830

minVisibleλ :: Word16
minVisibleλ = 360

{-# SPECIALIZE visibleBounds :: Spectrum Float -> Bounds1 Float #-}
{-# SPECIALIZE visibleBounds :: Spectrum Double -> Bounds1 Double #-}
visibleBounds :: (Bounded a, Ord a, Num a) => Spectrum a -> Bounds1 a
visibleBounds = \case
  ConstSpectrum a
    | a == 0 -> mempty
    | otherwise -> maxVisibleBounds
  AddSpectrum a b -> visibleBounds a <> visibleBounds b
  MulSpectrum a b -> visibleBounds a `Bounds.intersect` visibleBounds b
  SampledSpectrum λmin samples ->
    on
      Bounds
      (P . V1 . fromIntegral)
      (max minVisibleλ λmin)
      (min maxVisibleλ $ λmin + fromIntegral (V.length samples))
  InterpolatedSpectrum points
    | V.null points -> mempty
    | otherwise ->
        on Bounds (P . V1) (fst $ fst $ V.unsafeHead points) (fst $ fst $ V.unsafeLast points)
  BlackbodySpectrum _ -> maxVisibleBounds
  SigmoidQuadraticSpectrum{} -> maxVisibleBounds

{-# SPECIALIZE evalSpectrum :: Float -> Spectrum Float -> Float #-}
{-# SPECIALIZE evalSpectrum :: Double -> Spectrum Double -> Double #-}
evalSpectrum :: forall a. (RealFloat a) => a -> Spectrum a -> a
evalSpectrum λ
  | λ >= fromIntegral minVisibleλ && λ <= fromIntegral maxVisibleλ = go
  | otherwise = const 0
  where
    go = \case
      ConstSpectrum a -> a
      AddSpectrum a b -> on (+) go a b
      MulSpectrum a b -> on (*) go a b
      SigmoidQuadraticSpectrum a b c -> sigmoid $ a * (λ * λ) + b * λ + c
      SampledSpectrum λMin samples -> case round λ - fromIntegral λMin of
        offset
          | offset < 0 || offset >= V.length samples -> 0
          | otherwise -> samples V.! offset
      InterpolatedSpectrum points -> interpolateInterval λ points
      BlackbodySpectrum Blackbody{..}
        | λ <= 0 -> 0
        | otherwise -> blackbody bbTemperature λ * bbNormalizationFactor

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

lerpScalar :: (Eq a, Num a) => a -> a -> a -> a
lerpScalar 0 a _ = a
lerpScalar 1 _ b = b
lerpScalar t a b = (1 - t) * a + t * b

upperBound :: (RealFloat a) => Spectrum a -> a
upperBound = \case
  ConstSpectrum a -> a
  AddSpectrum a b -> upperBound a + upperBound b
  MulSpectrum a b -> upperBound a * upperBound b
  SigmoidQuadraticSpectrum a b c -> case compare a 0 of
    LT ->
      let v = -b / (2 * a)
       in sigmoid $ a * v * v + b * v + c
    EQ -> if b == 0 then sigmoid c else 1
    GT -> 1
  SampledSpectrum _ samples -> V.foldr max 0 samples
  InterpolatedSpectrum points -> maximum $ uncurry max . bimap snd snd <$> points
  BlackbodySpectrum _ -> 1

integrate :: forall a. (RealFloat a) => Bounds1 a -> Spectrum a -> a
integrate bounds@(Bounds (P (V1 a)) (P (V1 b))) s
  | Bounds.null bounds = 0
  | otherwise = case s of
      ConstSpectrum i -> i * range
      AddSpectrum s0 s1 -> integrate bounds s0 + integrate bounds s1
      MulSpectrum (ConstSpectrum x) y -> x * integrate bounds y
      MulSpectrum x (ConstSpectrum y) -> y * integrate bounds x
      MulSpectrum (SampledSpectrum λMin samples) (SampledSpectrum λMin' samples') -> do
        let go λMin0 samples0 λMin1 samples1 = do
              let d = fromIntegral $ λMin1 - λMin0
              integrate bounds $
                SampledSpectrum λMin1 $
                  V.zipWith (*) (V.drop d samples0) samples1
        case compare λMin λMin' of
          LT -> go λMin samples λMin' samples'
          EQ -> integrate bounds $ SampledSpectrum λMin $ V.zipWith (*) samples samples'
          GT -> go λMin' samples' λMin samples
      MulSpectrum (SampledSpectrum λMin samples) y ->
        integrate bounds $ SampledSpectrum λMin $ multiplySample (fromIntegral λMin) samples y
      MulSpectrum x (SampledSpectrum λMin samples) ->
        integrate bounds $ SampledSpectrum λMin $ multiplySample (fromIntegral λMin) samples x
      SampledSpectrum λMin samples -> do
        let a' = floor a
        let range' = ceiling b - a'
        let skip = max 0 $ a' - fromIntegral λMin
        V.sum $ V.slice skip range' samples
      _ -> foldr ((+) . flip evalSpectrum s . fromInteger) 0 [floor a .. ceiling b]
  where
    range = b - a

    multiplySample :: Int -> V.Vector a -> Spectrum a -> V.Vector a
    multiplySample λMin samples s' =
      V.generate (V.length samples) \i ->
        evalSpectrum (fromIntegral $ λMin + i) s' * (samples V.! i)
{-# INLINE integrate #-}

{-# SPECIALIZE integrateVisible :: Spectrum Float -> Float #-}
{-# SPECIALIZE integrateVisible :: Spectrum Double -> Double #-}
integrateVisible :: (RealFloat a, Bounded a) => Spectrum a -> a
integrateVisible s = integrate (visibleBounds s) s
{-# INLINE integrateVisible #-}

data WavelengthSample a = WavelengthSample
  { wsλ :: a
  , wsPdf :: a
  }
  deriving (Show, Read, Eq, Ord)

{-# SPECIALIZE sampleWavelengthsUniform :: Float -> Float -> Float -> Int -> [WavelengthSample Float] #-}
{-# SPECIALIZE sampleWavelengthsUniform :: Double -> Double -> Double -> Int -> [WavelengthSample Double] #-}
sampleWavelengthsUniform :: (Fractional a, Ord a) => a -> a -> a -> Int -> [WavelengthSample a]
sampleWavelengthsUniform u λMin λMax nSamples = do
  let range = λMax - λMin
  let λInitial = u * range
  let delta = range / fromIntegral nSamples
  let wsPdf = 1 / range
  i <- [0 .. nSamples]
  let λi = λInitial + fromIntegral i * delta
  pure
    WavelengthSample
      { wsλ = if λi > λMax then λMin + (λi - λMax) else λi
      , wsPdf
      }

{-# SPECIALIZE sampleWavelengthsVisible :: Float -> Int -> [WavelengthSample Float] #-}
{-# SPECIALIZE sampleWavelengthsVisible :: Double -> Int -> [WavelengthSample Double] #-}
sampleWavelengthsVisible :: (RealFloat a) => a -> Int -> [WavelengthSample a]
sampleWavelengthsVisible u nSamples = do
  let nInv = recip $ fromIntegral nSamples
  i <- [0 .. nSamples - 1]
  let t = fromIntegral i * nInv
  let (_ :: Integer, up) = properFraction $ u + t
  let wsλ = sampleVisible up
  pure $
    WavelengthSample
      { wsλ
      , wsPdf = visibleWavelengthsPdf wsλ
      }

{-# SPECIALIZE sampleVisible :: Float -> Float #-}
{-# SPECIALIZE sampleVisible :: Double -> Double #-}
sampleVisible :: (Floating a) => a -> a
sampleVisible u = 538 - 138.888889 * atanh (0.85691062 - 1.82750197 * u)

{-# SPECIALIZE visibleWavelengthsPdf :: Float -> Float #-}
{-# SPECIALIZE visibleWavelengthsPdf :: Double -> Double #-}
visibleWavelengthsPdf :: (Ord a, Floating a) => a -> a
visibleWavelengthsPdf λ
  | λ < fromIntegral minVisibleλ || λ > fromIntegral maxVisibleλ = 0
  | otherwise = 0.0039398042 / sqrt (cosh $ 0.0072 * (λ - 538))

mkXYZResponseCurve :: (Num a, Ord a) => [a] -> Spectrum a
mkXYZResponseCurve = sampledSpectrum minVisibleλ . V.fromList

{-# SPECIALIZE cieX :: Spectrum Float #-}
{-# SPECIALIZE cieX :: Spectrum Double #-}
cieX :: (Fractional a, Ord a) => Spectrum a
cieX =
  mkXYZResponseCurve
    [ 0.0001299
    , 0.000145847
    , 0.0001638021
    , 0.0001840037
    , 0.0002066902
    , 0.0002321
    , 0.000260728
    , 0.000293075
    , 0.000329388
    , 0.000369914
    , 0.0004149
    , 0.0004641587
    , 0.000518986
    , 0.000581854
    , 0.0006552347
    , 0.0007416
    , 0.0008450296
    , 0.0009645268
    , 0.001094949
    , 0.001231154
    , 0.001368
    , 0.00150205
    , 0.001642328
    , 0.001802382
    , 0.001995757
    , 0.002236
    , 0.002535385
    , 0.002892603
    , 0.003300829
    , 0.003753236
    , 0.004243
    , 0.004762389
    , 0.005330048
    , 0.005978712
    , 0.006741117
    , 0.00765
    , 0.008751373
    , 0.01002888
    , 0.0114217
    , 0.01286901
    , 0.01431
    , 0.01570443
    , 0.01714744
    , 0.01878122
    , 0.02074801
    , 0.02319
    , 0.02620736
    , 0.02978248
    , 0.03388092
    , 0.03846824
    , 0.04351
    , 0.0489956
    , 0.0550226
    , 0.0617188
    , 0.069212
    , 0.07763
    , 0.08695811
    , 0.09717672
    , 0.1084063
    , 0.1207672
    , 0.13438
    , 0.1493582
    , 0.1653957
    , 0.1819831
    , 0.198611
    , 0.21477
    , 0.2301868
    , 0.2448797
    , 0.2587773
    , 0.2718079
    , 0.2839
    , 0.2949438
    , 0.3048965
    , 0.3137873
    , 0.3216454
    , 0.3285
    , 0.3343513
    , 0.3392101
    , 0.3431213
    , 0.3461296
    , 0.34828
    , 0.3495999
    , 0.3501474
    , 0.350013
    , 0.349287
    , 0.34806
    , 0.3463733
    , 0.3442624
    , 0.3418088
    , 0.3390941
    , 0.3362
    , 0.3331977
    , 0.3300411
    , 0.3266357
    , 0.3228868
    , 0.3187
    , 0.3140251
    , 0.308884
    , 0.3032904
    , 0.2972579
    , 0.2908
    , 0.2839701
    , 0.2767214
    , 0.2689178
    , 0.2604227
    , 0.2511
    , 0.2408475
    , 0.2298512
    , 0.2184072
    , 0.2068115
    , 0.19536
    , 0.1842136
    , 0.1733273
    , 0.1626881
    , 0.1522833
    , 0.1421
    , 0.1321786
    , 0.1225696
    , 0.1132752
    , 0.1042979
    , 0.09564
    , 0.08729955
    , 0.07930804
    , 0.07171776
    , 0.06458099
    , 0.05795001
    , 0.05186211
    , 0.04628152
    , 0.04115088
    , 0.03641283
    , 0.03201
    , 0.0279172
    , 0.0241444
    , 0.020687
    , 0.0175404
    , 0.0147
    , 0.01216179
    , 0.00991996
    , 0.00796724
    , 0.006296346
    , 0.0049
    , 0.003777173
    , 0.00294532
    , 0.00242488
    , 0.002236293
    , 0.0024
    , 0.00292552
    , 0.00383656
    , 0.00517484
    , 0.00698208
    , 0.0093
    , 0.01214949
    , 0.01553588
    , 0.01947752
    , 0.02399277
    , 0.0291
    , 0.03481485
    , 0.04112016
    , 0.04798504
    , 0.05537861
    , 0.06327
    , 0.07163501
    , 0.08046224
    , 0.08973996
    , 0.09945645
    , 0.1096
    , 0.1201674
    , 0.1311145
    , 0.1423679
    , 0.1538542
    , 0.1655
    , 0.1772571
    , 0.18914
    , 0.2011694
    , 0.2133658
    , 0.2257499
    , 0.2383209
    , 0.2510668
    , 0.2639922
    , 0.2771017
    , 0.2904
    , 0.3038912
    , 0.3175726
    , 0.3314384
    , 0.3454828
    , 0.3597
    , 0.3740839
    , 0.3886396
    , 0.4033784
    , 0.4183115
    , 0.4334499
    , 0.4487953
    , 0.464336
    , 0.480064
    , 0.4959713
    , 0.5120501
    , 0.5282959
    , 0.5446916
    , 0.5612094
    , 0.5778215
    , 0.5945
    , 0.6112209
    , 0.6279758
    , 0.6447602
    , 0.6615697
    , 0.6784
    , 0.6952392
    , 0.7120586
    , 0.7288284
    , 0.7455188
    , 0.7621
    , 0.7785432
    , 0.7948256
    , 0.8109264
    , 0.8268248
    , 0.8425
    , 0.8579325
    , 0.8730816
    , 0.8878944
    , 0.9023181
    , 0.9163
    , 0.9297995
    , 0.9427984
    , 0.9552776
    , 0.9672179
    , 0.9786
    , 0.9893856
    , 0.9995488
    , 1.0090892
    , 1.0180064
    , 1.0263
    , 1.0339827
    , 1.040986
    , 1.047188
    , 1.0524667
    , 1.0567
    , 1.0597944
    , 1.0617992
    , 1.0628068
    , 1.0629096
    , 1.0622
    , 1.0607352
    , 1.0584436
    , 1.0552244
    , 1.0509768
    , 1.0456
    , 1.0390369
    , 1.0313608
    , 1.0226662
    , 1.0130477
    , 1.0026
    , 0.9913675
    , 0.9793314
    , 0.9664916
    , 0.9528479
    , 0.9384
    , 0.923194
    , 0.907244
    , 0.890502
    , 0.87292
    , 0.8544499
    , 0.835084
    , 0.814946
    , 0.794186
    , 0.772954
    , 0.7514
    , 0.7295836
    , 0.7075888
    , 0.6856022
    , 0.6638104
    , 0.6424
    , 0.6215149
    , 0.6011138
    , 0.5811052
    , 0.5613977
    , 0.5419
    , 0.5225995
    , 0.5035464
    , 0.4847436
    , 0.4661939
    , 0.4479
    , 0.4298613
    , 0.412098
    , 0.394644
    , 0.3775333
    , 0.3608
    , 0.3444563
    , 0.3285168
    , 0.3130192
    , 0.2980011
    , 0.2835
    , 0.2695448
    , 0.2561184
    , 0.2431896
    , 0.2307272
    , 0.2187
    , 0.2070971
    , 0.1959232
    , 0.1851708
    , 0.1748323
    , 0.1649
    , 0.1553667
    , 0.14623
    , 0.13749
    , 0.1291467
    , 0.1212
    , 0.1136397
    , 0.106465
    , 0.09969044
    , 0.09333061
    , 0.0874
    , 0.08190096
    , 0.07680428
    , 0.07207712
    , 0.06768664
    , 0.0636
    , 0.05980685
    , 0.05628216
    , 0.05297104
    , 0.04981861
    , 0.04677
    , 0.04378405
    , 0.04087536
    , 0.03807264
    , 0.03540461
    , 0.0329
    , 0.03056419
    , 0.02838056
    , 0.02634484
    , 0.02445275
    , 0.0227
    , 0.02108429
    , 0.01959988
    , 0.01823732
    , 0.01698717
    , 0.01584
    , 0.01479064
    , 0.01383132
    , 0.01294868
    , 0.0121292
    , 0.01135916
    , 0.01062935
    , 0.009938846
    , 0.009288422
    , 0.008678854
    , 0.008110916
    , 0.007582388
    , 0.007088746
    , 0.006627313
    , 0.006195408
    , 0.005790346
    , 0.005409826
    , 0.005052583
    , 0.004717512
    , 0.004403507
    , 0.004109457
    , 0.003833913
    , 0.003575748
    , 0.003334342
    , 0.003109075
    , 0.002899327
    , 0.002704348
    , 0.00252302
    , 0.002354168
    , 0.002196616
    , 0.00204919
    , 0.00191096
    , 0.001781438
    , 0.00166011
    , 0.001546459
    , 0.001439971
    , 0.001340042
    , 0.001246275
    , 0.001158471
    , 0.00107643
    , 0.0009999493
    , 0.0009287358
    , 0.0008624332
    , 0.0008007503
    , 0.000743396
    , 0.0006900786
    , 0.0006405156
    , 0.0005945021
    , 0.0005518646
    , 0.000512429
    , 0.0004760213
    , 0.0004424536
    , 0.0004115117
    , 0.0003829814
    , 0.0003566491
    , 0.0003323011
    , 0.0003097586
    , 0.0002888871
    , 0.0002695394
    , 0.0002515682
    , 0.0002348261
    , 0.000219171
    , 0.0002045258
    , 0.0001908405
    , 0.0001780654
    , 0.0001661505
    , 0.0001550236
    , 0.0001446219
    , 0.0001349098
    , 0.000125852
    , 0.000117413
    , 0.0001095515
    , 0.0001022245
    , 0.00009539445
    , 0.0000890239
    , 0.00008307527
    , 0.00007751269
    , 0.00007231304
    , 0.00006745778
    , 0.00006292844
    , 0.00005870652
    , 0.00005477028
    , 0.00005109918
    , 0.00004767654
    , 0.00004448567
    , 0.00004150994
    , 0.00003873324
    , 0.00003614203
    , 0.00003372352
    , 0.00003146487
    , 0.00002935326
    , 0.00002737573
    , 0.00002552433
    , 0.00002379376
    , 0.0000221787
    , 0.00002067383
    , 0.00001927226
    , 0.0000179664
    , 0.00001674991
    , 0.00001561648
    , 0.00001455977
    , 0.00001357387
    , 0.00001265436
    , 0.00001179723
    , 0.00001099844
    , 0.00001025398
    , 0.000009559646
    , 0.000008912044
    , 0.000008308358
    , 0.000007745769
    , 0.000007221456
    , 0.000006732475
    , 0.000006276423
    , 0.000005851304
    , 0.000005455118
    , 0.000005085868
    , 0.000004741466
    , 0.000004420236
    , 0.000004120783
    , 0.000003841716
    , 0.000003581652
    , 0.000003339127
    , 0.000003112949
    , 0.000002902121
    , 0.000002705645
    , 0.000002522525
    , 0.000002351726
    , 0.000002192415
    , 0.000002043902
    , 0.000001905497
    , 0.000001776509
    , 0.000001656215
    , 0.000001544022
    , 0.00000143944
    , 0.000001341977
    , 0.000001251141
    ]

{-# SPECIALIZE cieY :: Spectrum Float #-}
{-# SPECIALIZE cieY :: Spectrum Double #-}
cieY :: (Fractional a, Ord a) => Spectrum a
cieY =
  mkXYZResponseCurve
    [ 0.000003917
    , 0.000004393581
    , 0.000004929604
    , 0.000005532136
    , 0.000006208245
    , 0.000006965
    , 0.000007813219
    , 0.000008767336
    , 0.000009839844
    , 0.00001104323
    , 0.00001239
    , 0.00001388641
    , 0.00001555728
    , 0.00001744296
    , 0.00001958375
    , 0.00002202
    , 0.00002483965
    , 0.00002804126
    , 0.00003153104
    , 0.00003521521
    , 0.000039
    , 0.0000428264
    , 0.0000469146
    , 0.0000515896
    , 0.0000571764
    , 0.000064
    , 0.00007234421
    , 0.00008221224
    , 0.00009350816
    , 0.0001061361
    , 0.00012
    , 0.000134984
    , 0.000151492
    , 0.000170208
    , 0.000191816
    , 0.000217
    , 0.0002469067
    , 0.00028124
    , 0.00031852
    , 0.0003572667
    , 0.000396
    , 0.0004337147
    , 0.000473024
    , 0.000517876
    , 0.0005722187
    , 0.00064
    , 0.00072456
    , 0.0008255
    , 0.00094116
    , 0.00106988
    , 0.00121
    , 0.001362091
    , 0.001530752
    , 0.001720368
    , 0.001935323
    , 0.00218
    , 0.0024548
    , 0.002764
    , 0.0031178
    , 0.0035264
    , 0.004
    , 0.00454624
    , 0.00515932
    , 0.00582928
    , 0.00654616
    , 0.0073
    , 0.008086507
    , 0.00890872
    , 0.00976768
    , 0.01066443
    , 0.0116
    , 0.01257317
    , 0.01358272
    , 0.01462968
    , 0.01571509
    , 0.01684
    , 0.01800736
    , 0.01921448
    , 0.02045392
    , 0.02171824
    , 0.023
    , 0.02429461
    , 0.02561024
    , 0.02695857
    , 0.02835125
    , 0.0298
    , 0.03131083
    , 0.03288368
    , 0.03452112
    , 0.03622571
    , 0.038
    , 0.03984667
    , 0.041768
    , 0.043766
    , 0.04584267
    , 0.048
    , 0.05024368
    , 0.05257304
    , 0.05498056
    , 0.05745872
    , 0.06
    , 0.06260197
    , 0.06527752
    , 0.06804208
    , 0.07091109
    , 0.0739
    , 0.077016
    , 0.0802664
    , 0.0836668
    , 0.0872328
    , 0.09098
    , 0.09491755
    , 0.09904584
    , 0.1033674
    , 0.1078846
    , 0.1126
    , 0.117532
    , 0.1226744
    , 0.1279928
    , 0.1334528
    , 0.13902
    , 0.1446764
    , 0.1504693
    , 0.1564619
    , 0.1627177
    , 0.1693
    , 0.1762431
    , 0.1835581
    , 0.1912735
    , 0.199418
    , 0.20802
    , 0.2171199
    , 0.2267345
    , 0.2368571
    , 0.2474812
    , 0.2586
    , 0.2701849
    , 0.2822939
    , 0.2950505
    , 0.308578
    , 0.323
    , 0.3384021
    , 0.3546858
    , 0.3716986
    , 0.3892875
    , 0.4073
    , 0.4256299
    , 0.4443096
    , 0.4633944
    , 0.4829395
    , 0.503
    , 0.5235693
    , 0.544512
    , 0.56569
    , 0.5869653
    , 0.6082
    , 0.6293456
    , 0.6503068
    , 0.6708752
    , 0.6908424
    , 0.71
    , 0.7281852
    , 0.7454636
    , 0.7619694
    , 0.7778368
    , 0.7932
    , 0.8081104
    , 0.8224962
    , 0.8363068
    , 0.8494916
    , 0.862
    , 0.8738108
    , 0.8849624
    , 0.8954936
    , 0.9054432
    , 0.9148501
    , 0.9237348
    , 0.9320924
    , 0.9399226
    , 0.9472252
    , 0.954
    , 0.9602561
    , 0.9660074
    , 0.9712606
    , 0.9760225
    , 0.9803
    , 0.9840924
    , 0.9874812
    , 0.9903128
    , 0.9928116
    , 0.9949501
    , 0.9967108
    , 0.9980983
    , 0.999112
    , 0.9997482
    , 1
    , 0.9998567
    , 0.9993046
    , 0.9983255
    , 0.9968987
    , 0.995
    , 0.9926005
    , 0.9897426
    , 0.9864444
    , 0.9827241
    , 0.9786
    , 0.9740837
    , 0.9691712
    , 0.9638568
    , 0.9581349
    , 0.952
    , 0.9454504
    , 0.9384992
    , 0.9311628
    , 0.9234576
    , 0.9154
    , 0.9070064
    , 0.8982772
    , 0.8892048
    , 0.8797816
    , 0.87
    , 0.8598613
    , 0.849392
    , 0.838622
    , 0.8275813
    , 0.8163
    , 0.8047947
    , 0.793082
    , 0.781192
    , 0.7691547
    , 0.757
    , 0.7447541
    , 0.7324224
    , 0.7200036
    , 0.7074965
    , 0.6949
    , 0.6822192
    , 0.6694716
    , 0.6566744
    , 0.6438448
    , 0.631
    , 0.6181555
    , 0.6053144
    , 0.5924756
    , 0.5796379
    , 0.5668
    , 0.5539611
    , 0.5411372
    , 0.5283528
    , 0.5156323
    , 0.503
    , 0.4904688
    , 0.4780304
    , 0.4656776
    , 0.4534032
    , 0.4412
    , 0.42908
    , 0.417036
    , 0.405032
    , 0.393032
    , 0.381
    , 0.3689184
    , 0.3568272
    , 0.3447768
    , 0.3328176
    , 0.321
    , 0.3093381
    , 0.2978504
    , 0.2865936
    , 0.2756245
    , 0.265
    , 0.2547632
    , 0.2448896
    , 0.2353344
    , 0.2260528
    , 0.217
    , 0.2081616
    , 0.1995488
    , 0.1911552
    , 0.1829744
    , 0.175
    , 0.1672235
    , 0.1596464
    , 0.1522776
    , 0.1451259
    , 0.1382
    , 0.1315003
    , 0.1250248
    , 0.1187792
    , 0.1127691
    , 0.107
    , 0.1014762
    , 0.09618864
    , 0.09112296
    , 0.08626485
    , 0.0816
    , 0.07712064
    , 0.07282552
    , 0.06871008
    , 0.06476976
    , 0.061
    , 0.05739621
    , 0.05395504
    , 0.05067376
    , 0.04754965
    , 0.04458
    , 0.04175872
    , 0.03908496
    , 0.03656384
    , 0.03420048
    , 0.032
    , 0.02996261
    , 0.02807664
    , 0.02632936
    , 0.02470805
    , 0.0232
    , 0.02180077
    , 0.02050112
    , 0.01928108
    , 0.01812069
    , 0.017
    , 0.01590379
    , 0.01483718
    , 0.01381068
    , 0.01283478
    , 0.01192
    , 0.01106831
    , 0.01027339
    , 0.009533311
    , 0.008846157
    , 0.00821
    , 0.007623781
    , 0.007085424
    , 0.006591476
    , 0.006138485
    , 0.005723
    , 0.005343059
    , 0.004995796
    , 0.004676404
    , 0.004380075
    , 0.004102
    , 0.003838453
    , 0.003589099
    , 0.003354219
    , 0.003134093
    , 0.002929
    , 0.002738139
    , 0.002559876
    , 0.002393244
    , 0.002237275
    , 0.002091
    , 0.001953587
    , 0.00182458
    , 0.00170358
    , 0.001590187
    , 0.001484
    , 0.001384496
    , 0.001291268
    , 0.001204092
    , 0.001122744
    , 0.001047
    , 0.0009765896
    , 0.0009111088
    , 0.0008501332
    , 0.0007932384
    , 0.00074
    , 0.0006900827
    , 0.00064331
    , 0.000599496
    , 0.0005584547
    , 0.00052
    , 0.0004839136
    , 0.0004500528
    , 0.0004183452
    , 0.0003887184
    , 0.0003611
    , 0.0003353835
    , 0.0003114404
    , 0.0002891656
    , 0.0002684539
    , 0.0002492
    , 0.0002313019
    , 0.0002146856
    , 0.0001992884
    , 0.0001850475
    , 0.0001719
    , 0.0001597781
    , 0.0001486044
    , 0.0001383016
    , 0.0001287925
    , 0.00012
    , 0.0001118595
    , 0.0001043224
    , 0.0000973356
    , 0.00009084587
    , 0.0000848
    , 0.00007914667
    , 0.000073858
    , 0.000068916
    , 0.00006430267
    , 0.00006
    , 0.00005598187
    , 0.0000522256
    , 0.0000487184
    , 0.00004544747
    , 0.0000424
    , 0.00003956104
    , 0.00003691512
    , 0.00003444868
    , 0.00003214816
    , 0.00003
    , 0.00002799125
    , 0.00002611356
    , 0.00002436024
    , 0.00002272461
    , 0.0000212
    , 0.00001977855
    , 0.00001845285
    , 0.00001721687
    , 0.00001606459
    , 0.00001499
    , 0.00001398728
    , 0.00001305155
    , 0.00001217818
    , 0.00001136254
    , 0.0000106
    , 0.000009885877
    , 0.000009217304
    , 0.000008592362
    , 0.000008009133
    , 0.0000074657
    , 0.000006959567
    , 0.000006487995
    , 0.000006048699
    , 0.000005639396
    , 0.0000052578
    , 0.000004901771
    , 0.00000456972
    , 0.000004260194
    , 0.000003971739
    , 0.0000037029
    , 0.000003452163
    , 0.000003218302
    , 0.0000030003
    , 0.000002797139
    , 0.0000026078
    , 0.00000243122
    , 0.000002266531
    , 0.000002113013
    , 0.000001969943
    , 0.0000018366
    , 0.00000171223
    , 0.000001596228
    , 0.00000148809
    , 0.000001387314
    , 0.0000012934
    , 0.00000120582
    , 0.000001124143
    , 0.000001048009
    , 0.0000009770578
    , 0.00000091093
    , 0.0000008492513
    , 0.0000007917212
    , 0.0000007380904
    , 0.0000006881098
    , 0.00000064153
    , 0.0000005980895
    , 0.0000005575746
    , 0.000000519808
    , 0.0000004846123
    , 0.00000045181
    ]

{-# SPECIALIZE cieYIntegral :: Float #-}
{-# SPECIALIZE cieYIntegral :: Double #-}
cieYIntegral :: (RealFloat a, Bounded a) => a
cieYIntegral = integrateVisible cieY

{-# SPECIALIZE cieZ :: Spectrum Float #-}
{-# SPECIALIZE cieZ :: Spectrum Double #-}
cieZ :: (Fractional a, Ord a) => Spectrum a
cieZ =
  mkXYZResponseCurve
    [ 0.0006061
    , 0.0006808792
    , 0.0007651456
    , 0.0008600124
    , 0.0009665928
    , 0.001086
    , 0.001220586
    , 0.001372729
    , 0.001543579
    , 0.001734286
    , 0.001946
    , 0.002177777
    , 0.002435809
    , 0.002731953
    , 0.003078064
    , 0.003486
    , 0.003975227
    , 0.00454088
    , 0.00515832
    , 0.005802907
    , 0.006450001
    , 0.007083216
    , 0.007745488
    , 0.008501152
    , 0.009414544
    , 0.01054999
    , 0.0119658
    , 0.01365587
    , 0.01558805
    , 0.01773015
    , 0.02005001
    , 0.02251136
    , 0.02520288
    , 0.02827972
    , 0.03189704
    , 0.03621
    , 0.04143771
    , 0.04750372
    , 0.05411988
    , 0.06099803
    , 0.06785001
    , 0.07448632
    , 0.08136156
    , 0.08915364
    , 0.09854048
    , 0.1102
    , 0.1246133
    , 0.1417017
    , 0.1613035
    , 0.1832568
    , 0.2074
    , 0.2336921
    , 0.2626114
    , 0.2947746
    , 0.3307985
    , 0.3713
    , 0.4162091
    , 0.4654642
    , 0.5196948
    , 0.5795303
    , 0.6456
    , 0.7184838
    , 0.7967133
    , 0.8778459
    , 0.959439
    , 1.0390501
    , 1.1153673
    , 1.1884971
    , 1.2581233
    , 1.3239296
    , 1.3856
    , 1.4426352
    , 1.4948035
    , 1.5421903
    , 1.5848807
    , 1.62296
    , 1.6564048
    , 1.6852959
    , 1.7098745
    , 1.7303821
    , 1.74706
    , 1.7600446
    , 1.7696233
    , 1.7762637
    , 1.7804334
    , 1.7826
    , 1.7829682
    , 1.7816998
    , 1.7791982
    , 1.7758671
    , 1.77211
    , 1.7682589
    , 1.764039
    , 1.7589438
    , 1.7524663
    , 1.7441
    , 1.7335595
    , 1.7208581
    , 1.7059369
    , 1.6887372
    , 1.6692
    , 1.6475287
    , 1.6234127
    , 1.5960223
    , 1.564528
    , 1.5281
    , 1.4861114
    , 1.4395215
    , 1.3898799
    , 1.3387362
    , 1.28764
    , 1.2374223
    , 1.1878243
    , 1.1387611
    , 1.090148
    , 1.0419
    , 0.9941976
    , 0.9473473
    , 0.9014531
    , 0.8566193
    , 0.8129501
    , 0.7705173
    , 0.7294448
    , 0.6899136
    , 0.6521049
    , 0.6162
    , 0.5823286
    , 0.5504162
    , 0.5203376
    , 0.4919673
    , 0.46518
    , 0.4399246
    , 0.4161836
    , 0.3938822
    , 0.3729459
    , 0.3533
    , 0.3348578
    , 0.3175521
    , 0.3013375
    , 0.2861686
    , 0.272
    , 0.2588171
    , 0.2464838
    , 0.2347718
    , 0.2234533
    , 0.2123
    , 0.2011692
    , 0.1901196
    , 0.1792254
    , 0.1685608
    , 0.1582
    , 0.1481383
    , 0.1383758
    , 0.1289942
    , 0.1200751
    , 0.1117
    , 0.1039048
    , 0.09666748
    , 0.08998272
    , 0.08384531
    , 0.07824999
    , 0.07320899
    , 0.06867816
    , 0.06456784
    , 0.06078835
    , 0.05725001
    , 0.05390435
    , 0.05074664
    , 0.04775276
    , 0.04489859
    , 0.04216
    , 0.03950728
    , 0.03693564
    , 0.03445836
    , 0.03208872
    , 0.02984
    , 0.02771181
    , 0.02569444
    , 0.02378716
    , 0.02198925
    , 0.0203
    , 0.01871805
    , 0.01724036
    , 0.01586364
    , 0.01458461
    , 0.0134
    , 0.01230723
    , 0.01130188
    , 0.01037792
    , 0.009529306
    , 0.008749999
    , 0.0080352
    , 0.0073816
    , 0.0067854
    , 0.0062428
    , 0.005749999
    , 0.0053036
    , 0.0048998
    , 0.0045342
    , 0.0042024
    , 0.0039
    , 0.0036232
    , 0.0033706
    , 0.0031414
    , 0.0029348
    , 0.002749999
    , 0.0025852
    , 0.0024386
    , 0.0023094
    , 0.0021968
    , 0.0021
    , 0.002017733
    , 0.0019482
    , 0.0018898
    , 0.001840933
    , 0.0018
    , 0.001766267
    , 0.0017378
    , 0.0017112
    , 0.001683067
    , 0.001650001
    , 0.001610133
    , 0.0015644
    , 0.0015136
    , 0.001458533
    , 0.0014
    , 0.001336667
    , 0.00127
    , 0.001205
    , 0.001146667
    , 0.0011
    , 0.0010688
    , 0.0010494
    , 0.0010356
    , 0.0010212
    , 0.001
    , 0.00096864
    , 0.00092992
    , 0.00088688
    , 0.00084256
    , 0.0008
    , 0.00076096
    , 0.00072368
    , 0.00068592
    , 0.00064544
    , 0.0006
    , 0.0005478667
    , 0.0004916
    , 0.0004354
    , 0.0003834667
    , 0.00034
    , 0.0003072533
    , 0.00028316
    , 0.00026544
    , 0.0002518133
    , 0.00024
    , 0.0002295467
    , 0.00022064
    , 0.00021196
    , 0.0002021867
    , 0.00019
    , 0.0001742133
    , 0.00015564
    , 0.00013596
    , 0.0001168533
    , 0.0001
    , 0.00008613333
    , 0.0000746
    , 0.000065
    , 0.00005693333
    , 0.00004999999
    , 0.00004416
    , 0.00003948
    , 0.00003572
    , 0.00003264
    , 0.00003
    , 0.00002765333
    , 0.00002556
    , 0.00002364
    , 0.00002181333
    , 0.00002
    , 0.00001813333
    , 0.0000162
    , 0.0000142
    , 0.00001213333
    , 0.00001
    , 0.000007733333
    , 0.0000054
    , 0.0000032
    , 0.000001333333
    ]

mkDSeriesIlluminant
  :: (RealFloat a, Bounded a, Enum a)
  => Bool
  -> [a]
  -> Spectrum a
mkDSeriesIlluminant shouldNormalize =
  (if shouldNormalize then normalizedInterpolatedSpectrum else interpolatedSpectrum)
    . zip [300, 305 ..]

{-# SPECIALIZE cieD50 :: Spectrum Float #-}
{-# SPECIALIZE cieD50 :: Spectrum Double #-}
cieD50 :: (RealFloat a, Bounded a, Enum a) => Spectrum a
cieD50 =
  mkDSeriesIlluminant
    True
    [ 0.0192
    , 1.0366
    , 2.054
    , 4.913
    , 7.772
    , 11.2557
    , 14.7395
    , 16.339001
    , 17.938601
    , 19.4667
    , 20.9949
    , 22.459999
    , 23.9251
    , 25.433901
    , 26.942699
    , 25.701799
    , 24.461
    , 27.1507
    , 29.840401
    , 39.550301
    , 49.664001
    , 53.155998
    , 56.647999
    , 58.445999
    , 60.243999
    , 59.23
    , 58.216
    , 66.973999
    , 75.732002
    , 81.998001
    , 88.264
    , 89.93
    , 91.596001
    , 91.940002
    , 92.283997
    , 94.155998
    , 96.028
    , 94.311996
    , 92.596001
    , 94.424004
    , 96.251999
    , 96.662003
    , 97.071999
    , 97.314003
    , 97.556
    , 100.005997
    , 102.456001
    , 101.694
    , 100.931999
    , 101.678001
    , 102.424004
    , 101.211998
    , 100
    , 98.036697
    , 96.073402
    , 95.678398
    , 95.283501
    , 92.577103
    , 89.870697
    , 90.772499
    , 91.6744
    , 91.739502
    , 91.804703
    , 90.964798
    , 90.124901
    , 87.998299
    , 85.871696
    , 86.715302
    , 87.558899
    , 86.069
    , 84.579102
    , 85.167603
    , 85.756203
    , 87.126404
    , 88.496597
    , 86.769997
    , 85.043404
    , 79.994698
    , 74.946098
    , 76.384598
    , 77.823196
    , 78.671303
    , 79.519501
    , 72.694199
    , 65.869003
    , 70.1791
    , 74.489197
    , 77.212601
    , 79.935997
    , 73.797401
    , 67.658897
    , 58.633598
    , 49.608398
    , 60.462101
    , 71.315804
    , 69.405701
    , 67.495598
    , 68.032303
    , 68.569
    , 65.9589
    , 63.348801
    , 59.333599
    , 55.318501
    , 58.2286
    , 61.138699
    , 62.712101
    , 64.2855
    ]

{-# SPECIALIZE acesD60 :: Spectrum Float #-}
{-# SPECIALIZE acesD60 :: Spectrum Double #-}
acesD60 :: (RealFloat a, Bounded a, Enum a) => Spectrum a
acesD60 =
  mkDSeriesIlluminant
    True
    [ 0.02928
    , 1.28964
    , 2.55
    , 9.0338
    , 15.5176
    , 21.94705
    , 28.3765
    , 29.93335
    , 31.4902
    , 33.75765
    , 36.0251
    , 37.2032
    , 38.3813
    , 40.6445
    , 42.9077
    , 42.05735
    , 41.207
    , 43.8121
    , 46.4172
    , 59.26285
    , 72.1085
    , 76.1756
    , 80.2427
    , 81.4878
    , 82.7329
    , 80.13505
    , 77.5372
    , 86.5577
    , 95.5782
    , 101.72045
    , 107.8627
    , 108.67115
    , 109.4796
    , 108.5873
    , 107.695
    , 108.6598
    , 109.6246
    , 106.6426
    , 103.6606
    , 104.42795
    , 105.1953
    , 104.7974
    , 104.3995
    , 103.45635
    , 102.5132
    , 104.2813
    , 106.0494
    , 104.67885
    , 103.3083
    , 103.4228
    , 103.5373
    , 101.76865
    , 100
    , 98.3769
    , 96.7538
    , 96.73515
    , 96.7165
    , 93.3013
    , 89.8861
    , 90.91705
    , 91.948
    , 91.98965
    , 92.0313
    , 91.3008
    , 90.5703
    , 88.5077
    , 86.4451
    , 86.9551
    , 87.4651
    , 85.6558
    , 83.8465
    , 84.20755
    , 84.5686
    , 85.9432
    , 87.3178
    , 85.3068
    , 83.2958
    , 78.66005
    , 74.0243
    , 75.23535
    , 76.4464
    , 77.67465
    , 78.9029
    , 72.12575
    , 65.3486
    , 69.6609
    , 73.9732
    , 76.6802
    , 79.3872
    , 73.28855
    , 67.1899
    , 58.18595
    , 49.182
    , 59.9723
    , 70.7626
    , 68.9039
    , 67.0452
    , 67.5469
    , 68.0486
    , 65.4631
    , 62.8776
    , 58.88595
    , 54.8943
    , 57.8066
    , 60.7189
    , 62.2491
    , 63.7793
    ]

{-# SPECIALIZE cieD65 :: Spectrum Float #-}
{-# SPECIALIZE cieD65 :: Spectrum Double #-}
cieD65 :: (RealFloat a, Bounded a, Enum a) => Spectrum a
cieD65 =
  mkDSeriesIlluminant
    True
    [ 0.0341
    , 1.6643
    , 3.2945
    , 11.7652
    , 20.236
    , 28.644699
    , 37.053501
    , 38.501099
    , 39.948799
    , 42.430199
    , 44.911701
    , 45.775002
    , 46.638302
    , 49.363701
    , 52.0891
    , 51.032299
    , 49.975498
    , 52.311798
    , 54.648201
    , 68.7015
    , 82.754898
    , 87.120399
    , 91.486
    , 92.4589
    , 93.431801
    , 90.056999
    , 86.682297
    , 95.773598
    , 104.864998
    , 110.935997
    , 117.008003
    , 117.410004
    , 117.811996
    , 116.335999
    , 114.861
    , 115.391998
    , 115.922997
    , 112.366997
    , 108.810997
    , 109.082001
    , 109.353996
    , 108.578003
    , 107.802002
    , 106.295998
    , 104.790001
    , 106.238998
    , 107.689003
    , 106.046997
    , 104.404999
    , 104.224998
    , 104.045998
    , 102.023003
    , 100
    , 98.167099
    , 96.334198
    , 96.061096
    , 95.788002
    , 92.236801
    , 88.6856
    , 89.345901
    , 90.006203
    , 89.802597
    , 89.599098
    , 88.648903
    , 87.6987
    , 85.493599
    , 83.288597
    , 83.493896
    , 83.699203
    , 81.862999
    , 80.026802
    , 80.120697
    , 80.2146
    , 81.246201
    , 82.277802
    , 80.280998
    , 78.284203
    , 74.002701
    , 69.721298
    , 70.665199
    , 71.6091
    , 72.978996
    , 74.348999
    , 67.976501
    , 61.604
    , 65.744797
    , 69.885597
    , 72.486298
    , 75.086998
    , 69.339798
    , 63.592701
    , 55.005402
    , 46.418201
    , 56.611801
    , 66.805397
    , 65.094101
    , 63.382801
    , 63.843399
    , 64.304001
    , 61.877899
    , 59.4519
    , 55.705399
    , 51.959
    , 54.699799
    , 57.440601
    , 58.876499
    , 60.3125
    ]

mkInterpolatedSpectrumFromInterleaved
  :: (RealFloat a, Bounded a)
  => Bool
  -> [a]
  -> Spectrum a
mkInterpolatedSpectrumFromInterleaved shouldNormalize =
  (if shouldNormalize then normalizedInterpolatedSpectrum else interpolatedSpectrum)
    . pairs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "pairs: uneven number of elements"
pairs (a : b : as) = (a, b) : pairs as
