module Numeric.Integration.MonteCarlo where

import Control.Comonad
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Profunctor (Profunctor)
import Data.Profunctor.Types (Profunctor (..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

type IntegratorT f s a = (s -> a) -> EstimatorT f s a
type Integrator2T f s t a = (s -> t -> a) -> EstimatorT f (s, t) a
type Integrator3T f s t u a = (s -> t -> u -> a) -> EstimatorT f (s, t, u) a
type Integrator s a = IntegratorT Identity s a
type Integrator2 s t a = Integrator2T Identity s t a
type Integrator3 s t u a = Integrator3T Identity s t u a

data EstimatorT f s a = EstimatorT a (V.Vector (f s) -> EstimatorT f s a)
  deriving (Functor)

type Estimator = EstimatorT Identity

{-# COMPLETE Estimator #-}

pattern Estimator :: a -> (V.Vector s -> Estimator s a) -> Estimator s a
pattern Estimator a next <- EstimatorT a ((. fmap Identity) -> next)
  where
    Estimator a next = EstimatorT a $ next . fmap runIdentity

instance (Functor f) => Profunctor (EstimatorT f) where
  dimap f g (EstimatorT a next) = EstimatorT (g a) $ dimap f g . next . (fmap . fmap) f

instance Comonad (EstimatorT f s) where
  extract (EstimatorT a _) = a
  duplicate e@(EstimatorT _ next) = EstimatorT e $ duplicate . next

instance Applicative (EstimatorT f s) where
  pure a = EstimatorT a $ const $ pure a
  EstimatorT f f' <*> EstimatorT a y' = EstimatorT (f a) $ liftA2 (<*>) f' y'

addSamplesT :: V.Vector (f s) -> EstimatorT f s y -> EstimatorT f s y
addSamplesT s (EstimatorT _ next) = next s

addSamples :: V.Vector s -> Estimator s y -> Estimator s y
addSamples = addSamplesT . fmap Identity

addSampleT :: f s -> EstimatorT f s y -> EstimatorT f s y
addSampleT = addSamplesT . pure

addSample :: s -> Estimator s y -> Estimator s y
addSample = addSamples . pure

lmapEstimatorT :: (g t -> f s) -> EstimatorT f s a -> EstimatorT g t a
lmapEstimatorT f (EstimatorT a next) = EstimatorT a $ lmapEstimatorT f . next . fmap f

hoistEstimatorT :: (forall x. g x -> f x) -> EstimatorT f s a -> EstimatorT g s a
hoistEstimatorT = lmapEstimatorT

-- Uniform estimators

uniformIntegrator :: (Fractional a, Real a, Ord b, Fractional b) => a -> a -> Integrator a b
uniformIntegrator a0 a1 integrand
  | range <= 0 = pure 0
  | otherwise = Estimator 0 $ go 0 0
  where
    go i acc samples
      | any outsideDomain samples = error "uniformIntegrator: sample is outside domain"
      | otherwise =
          let bs = integrand <$> samples
              acc' = acc + sum bs
              i' = i + V.length samples
           in Estimator (range * acc' / fromIntegral i') $ go i' acc'
    range = realToFrac (a1 - a0)
    outsideDomain a = a < a0 || a > a1

uniformIntegrator2
  :: (Fractional a, Real a, Fractional b, Real b, Ord c, Fractional c)
  => a
  -> a
  -> b
  -> b
  -> Integrator2 a b c
uniformIntegrator2 a0 a1 b0 b1 integrand
  | range <= 0 = pure 0
  | otherwise = Estimator 0 $ go 0 0
  where
    go i acc samples
      | any outsideDomain samples = error "uniformIntegrator2: sample is outside domain"
      | otherwise =
          let cs = uncurry integrand <$> samples
              acc' = acc + sum cs
              i' = i + V.length samples
           in Estimator (range * acc' / fromIntegral i') $ go i' acc'
    range = realToFrac (a1 - a0) * realToFrac (b1 - b0)
    outsideDomain (a, b) = a < a0 || a > a1 || b < b0 || b > b1

uniformIntegrator3
  :: (Fractional a, Real a, Fractional b, Real b, Fractional c, Real c, Ord d, Fractional d)
  => a
  -> a
  -> b
  -> b
  -> c
  -> c
  -> Integrator3 a b c d
uniformIntegrator3 a0 a1 b0 b1 c0 c1 integrand
  | range <= 0 = pure 0
  | otherwise = Estimator 0 $ go 0 0
  where
    go i acc samples
      | any outsideDomain samples = error "uniformIntegrator3: sample is outside domain"
      | otherwise =
          let ds = uncurry3 integrand <$> samples
              acc' = acc + sum ds
              i' = i + V.length samples
           in Estimator (range * acc' / fromIntegral i') $ go i' acc'
    range = realToFrac (a1 - a0) * realToFrac (b1 - b0) * realToFrac (c1 - c0)
    outsideDomain (a, b, c) = a < a0 || a > a1 || b < b0 || b > b1 || c < c0 || c > c1

-- -- Weighted estimators (importance sampling)

data WeightedSample p a = WeightedSample
  { weightedSample :: a
  , weightedSamplePDF :: p
  }
  deriving (Functor)

weightedIntegrator :: (Fractional b, Real p) => IntegratorT (WeightedSample p) a b
weightedIntegrator integrand = EstimatorT 0 $ go 0 0
  where
    go i acc samples = EstimatorT (acc' / i) $ go (i + 1) acc'
      where
        acc' =
          acc + sum do
            WeightedSample{..} <- samples
            when (weightedSamplePDF <= 0) $ error "weightedIntegrator: sample value has non-positive probability density"
            pure $ integrand weightedSample / realToFrac weightedSamplePDF

weightedIntegrator2 :: (Fractional c, Real p) => Integrator2T (WeightedSample p) a b c
weightedIntegrator2 integrand = weightedIntegrator $ uncurry integrand

weightedIntegrator3 :: (Fractional d, Real p) => Integrator3T (WeightedSample p) a b c d
weightedIntegrator3 integrand = weightedIntegrator $ uncurry3 integrand

-- Multiple-importance sampling

data MisSample p w a = MisSample
  { misSample :: a
  , misSampleIndex :: Int
  , misSamplePDF :: p
  , misSampleWeight :: w
  }
  deriving (Functor)

data MisState a p w b = MisState
  { misStateSampleCount :: Int
  , misStateTotal :: b
  }

misStateContribution :: (Fractional b) => MisState a p w b -> b
misStateContribution MisState{..} = misStateTotal / fromIntegral misStateSampleCount

misIntegrator :: (Fractional b, Real p, Real w) => Int -> IntegratorT (MisSample p w) a b
misIntegrator distributionCount integrand =
  EstimatorT 0 $ go $ V.replicate distributionCount $ MisState 0 0
  where
    go states samples = EstimatorT (sum $ misStateContribution <$> states') $ go states'
      where
        states' =
          V.modify
            ( \mStates ->
                for_ samples \MisSample{..} ->
                  if misSamplePDF <= 0
                    then error "misIntegrator: sample has non-positive probability density"
                    else do
                      let p = realToFrac misSamplePDF
                      let w = realToFrac misSampleWeight
                      let b =
                            if misSampleWeight == 0
                              then 0
                              else w * integrand misSample / p
                      let updateState MisState{..} =
                            MisState
                              { misStateSampleCount = succ misStateSampleCount
                              , misStateTotal = misStateTotal + b
                              , ..
                              }
                      M.modify mStates updateState misSampleIndex
            )
            states

misIntegrator2 :: (Fractional c, Real p, Real w) => Int -> Integrator2T (MisSample p w) a b c
misIntegrator2 distributions = misIntegrator distributions . uncurry

misIntegrator3 :: (Fractional d, Real p, Real w) => Int -> Integrator3T (MisSample p w) a b c d
misIntegrator3 distributions integrand = misIntegrator distributions \(a, b, c) -> integrand a b c

powerHeuristic :: (Floating p) => p -> Int -> V.Vector (Int, p) -> p
powerHeuristic beta distribution distributions =
  distributionFactor (distributions V.! distribution)
    / sum (distributionFactor <$> distributions)
  where
    distributionFactor (n, pdf) = (fromIntegral n * pdf) ** beta

balanceHeuristic :: (Fractional p) => Int -> V.Vector (Int, p) -> p
balanceHeuristic distribution distributions =
  distributionFactor (distributions V.! distribution)
    / sum (distributionFactor <$> distributions)
  where
    distributionFactor (n, pdf) = fromIntegral n * pdf

-- Russian Roulette

data RussianRouletteSample f p a = RussianRouletteSample
  { russianRouletteSample :: f a
  , russianRouletteProbability :: p
  , russianRouletteUniformVariable :: p
  }
  deriving (Functor)

russianRouletteIntegrator
  :: (Real p, Fractional b, Functor f)
  => b
  -> (forall s. IntegratorT f s b)
  -> IntegratorT (RussianRouletteSample f p) a b
russianRouletteIntegrator c integrator integrand =
  lmapEstimatorT
    (\(RussianRouletteSample fa q u) -> (\a -> RussianRouletteSample (Identity a) q u) <$> fa)
    $ integrator \case
      RussianRouletteSample (Identity a) q u
        | u > q -> let q' = realToFrac q in (integrand a - q' * c) / (1 - q')
        | otherwise -> c

russianRouletteIntegrator2
  :: (Real p, Fractional c, Functor f)
  => c
  -> (forall s. IntegratorT f s c)
  -> Integrator2T (RussianRouletteSample f p) a b c
russianRouletteIntegrator2 c integrator = russianRouletteIntegrator c integrator . uncurry

russianRouletteIntegrator3
  :: (Real p, Fractional d, Functor f)
  => d
  -> (forall s. IntegratorT f s d)
  -> Integrator3T (RussianRouletteSample f p) a b c d
russianRouletteIntegrator3 c integrator = russianRouletteIntegrator c integrator . uncurry3

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

foo :: (Real p) => IntegratorT (RussianRouletteSample (WeightedSample Float) p) s Float
foo = russianRouletteIntegrator 0 weightedIntegrator
