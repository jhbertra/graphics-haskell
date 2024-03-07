module Numeric.Interval.IEEE (
  (...),
  Interval,
  exactly,
  fromMidpointAndMargin,
  inf,
  magnitude,
  margin,
  member,
  midpoint,
  mignitude,
  overlaps,
  sup,
  width,
  square,
  mulPow2,
  quadratic,
  singleton,
  gamma,
) where

import Numeric.IEEE (IEEE (..))

data Interval a = I !a !a
  deriving (Show, Eq, Ord)

{-# SPECIALIZE gamma :: Integer -> Float #-}
{-# SPECIALIZE gamma :: Integer -> Double #-}
gamma :: (IEEE a) => Integer -> a
gamma n = epsilon / (2 / fromInteger n - epsilon)
{-# INLINE gamma #-}

(...) :: (Ord a) => a -> a -> Interval a
a ... b = I (min a b) (max a b)

sup :: Interval a -> a
sup (I _ a) = a
{-# INLINE sup #-}

inf :: Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

midpoint :: (Fractional a) => Interval a -> a
midpoint (I a b) = a + margin (I a b)
{-# INLINE midpoint #-}

margin :: (Fractional a) => Interval a -> a
margin b = width b * 0.5
{-# INLINE margin #-}

magnitude :: (IEEE a) => Interval a -> a
magnitude = sup . abs
{-# INLINE magnitude #-}

mignitude :: (IEEE a) => Interval a -> a
mignitude = inf . abs
{-# INLINE mignitude #-}

width :: (Num a) => Interval a -> a
width (I a b) = b - a
{-# INLINE width #-}

exactly :: (Eq a) => a -> Interval a -> Bool
exactly a (I lo hi) = a == lo && a == hi
{-# INLINE exactly #-}

member :: (Ord a) => a -> Interval a -> Bool
member a (I lo hi) = a >= lo && a <= hi
{-# INLINE member #-}

overlaps :: (Ord a) => Interval a -> Interval a -> Bool
overlaps (I lo hi) (I lo' hi') = lo <= hi' && hi >= lo'
{-# INLINE overlaps #-}

fromMidpointAndMargin :: (IEEE a) => a -> a -> Interval a
fromMidpointAndMargin a 0 = singleton a
fromMidpointAndMargin a m = I (subRoundDown a m) (addRoundUp a m)
{-# INLINE fromMidpointAndMargin #-}

singleton :: a -> Interval a
singleton a = I a a
{-# INLINE singleton #-}

increasing :: (a -> b) -> Interval a -> Interval b
increasing f (I a b) = I (f a) (f b)
{-# INLINE increasing #-}

increasingOut :: (IEEE b) => (a -> b) -> Interval a -> Interval b
increasingOut f (I a b) = I (predIEEE $ f a) (succIEEE $ f b)
{-# INLINE increasingOut #-}

decreasing :: (a -> b) -> Interval a -> Interval b
decreasing f (I a b) = I (f b) (f a)
{-# INLINE decreasing #-}

decreasingOut :: (IEEE b) => (a -> b) -> Interval a -> Interval b
decreasingOut f (I a b) = I (predIEEE $ f b) (succIEEE $ f a)
{-# INLINE decreasingOut #-}

instance (IEEE a) => Num (Interval a) where
  I a b + I a' b' =
    I
      (addRoundDown a a')
      (addRoundUp b b')
  {-# INLINE (+) #-}
  I a b - I a' b' =
    I
      (subRoundDown a b')
      (subRoundUp b a')
  {-# INLINE (-) #-}
  negate = decreasing negate
  {-# INLINE negate #-}
  I a a' * I b b' =
    I
      (minimum $ mulRoundDown <$> [a, b] <*> [a', b'])
      (maximum $ mulRoundUp <$> [a, b] <*> [a', b'])
  {-# INLINE (*) #-}
  abs i@(I a b)
    | a >= 0 = i
    | b <= 0 = negate i
    | otherwise = I 0 $ max (-a) b
  {-# INLINE abs #-}
  signum = increasing signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}

instance (IEEE a) => Real (Interval a) where
  toRational (I a b) = ra + (rb - ra) / 2
    where
      ra = toRational a
      rb = toRational b
  {-# INLINE toRational #-}

instance (IEEE a) => Fractional (Interval a) where
  fromRational = singleton . fromRational
  {-# INLINE fromRational #-}
  I a b / I a' b'
    | a' <= 0 && b' >= 0 =
        if a <= 0 && b >= 0
          then I nan nan
          else I (negate infinity) infinity
    | otherwise =
        I
          (minimum $ divRoundDown <$> [a, b] <*> [a', b'])
          (maximum $ divRoundUp <$> [a, b] <*> [a', b'])

instance (IEEE a) => RealFrac (Interval a) where
  properFraction x = (b, x - fromIntegral b)
    where
      b = truncate (midpoint x)
  {-# INLINE properFraction #-}
  ceiling = ceiling . sup
  {-# INLINE ceiling #-}
  floor = floor . inf
  {-# INLINE floor #-}
  round = round . midpoint
  {-# INLINE round #-}
  truncate = truncate . midpoint
  {-# INLINE truncate #-}

instance (IEEE a) => Floating (Interval a) where
  pi = singleton pi
  {-# INLINE pi #-}
  sqrt (I a b) = I (sqrtRoundDown a) (sqrtRoundUp b)
  {-# INLINE sqrt #-}
  exp = increasingOut exp
  {-# INLINE exp #-}
  log (I a b) = (if a > 0 then log a else -infinity) ... (if b > 0 then log b else -infinity)
  {-# INLINE log #-}
  sin = periodic (2 * pi) (fromMidpointAndMargin 0 1) (signum' . cos) sin
  {-# INLINE sin #-}
  cos = periodic (2 * pi) (fromMidpointAndMargin 0 1) (signum' . negate . sin) cos
  {-# INLINE cos #-}
  tan = periodic pi (-infinity ... infinity) (const GT) tan
  {-# INLINE tan #-}
  asin (I a b) = roundOut (fromMidpointAndMargin 0 halfPi) (asin' a) (asin' b)
    where
      halfPi = pi / 2
      asin' x
        | x >= 1 = halfPi
        | x <= -1 = -halfPi
        | otherwise = asin x
  {-# INLINE asin #-}
  acos (I a b) = roundOut (I 0 pi) (acos' a) (acos' b)
    where
      acos' x
        | x >= 1 = 0
        | x <= -1 = pi
        | otherwise = acos x
  {-# INLINE acos #-}
  atan = increasingOut atan
  {-# INLINE atan #-}
  sinh = increasingOut sinh
  {-# INLINE sinh #-}
  cosh x@(I a b)
    | b < 0 = decreasingOut cosh x
    | a >= 0 = increasingOut cosh x
    | otherwise = I 0 $ cosh if -a > b then a else b
  {-# INLINE cosh #-}
  tanh = increasingOut tanh
  {-# INLINE tanh #-}
  asinh = increasingOut asinh
  {-# INLINE asinh #-}
  acosh (I a b) = roundOut (I 0 infinity) (acosh' a) (acosh' b)
    where
      acosh' x
        | x <= 1 = 0
        | otherwise = acosh x
  {-# INLINE acosh #-}
  atanh (I a b) = roundOut (I (-infinity) infinity) (atanh' a) (atanh' b)
    where
      atanh' x
        | x <= -1 = -infinity
        | x >= 1 = infinity
        | otherwise = atanh x
  {-# INLINE atanh #-}

square :: (IEEE a) => Interval a -> Interval a
square i
  | a == 0 = I 0 $ mulRoundUp b b
  | otherwise = I (mulRoundUp a a) (mulRoundUp b b)
  where
    I a b = abs i

mulPow2 :: (IEEE a) => a -> Interval a -> Interval a
mulPow2 e (I a b)
  | isPow2OrZero = (a * e) ... (a * b)
  | otherwise = error "mulPow2: not a power of 2"
  where
    isPow2OrZero =
      e == 0 || abs (significand e) == recip (fromInteger $ floatRadix e)

quadratic :: (IEEE a) => Interval a -> Interval a -> Interval a -> Maybe (Interval a, Interval a)
quadratic a b c
  | d < 0 = Nothing
  | t0 > t1 = Just (t1, t0)
  | otherwise = Just (t0, t1)
  where
    d = b * b - mulPow2 4 a * c
    rootD = sqrt d
    q
      | midpoint b < 0 = mulPow2 (-0.5) $ b - rootD
      | otherwise = mulPow2 (-0.5) $ b + rootD
    t0 = q / a
    t1 = c / q

addRoundUp :: (IEEE a) => a -> a -> a
addRoundUp a b = succIEEE $ a + b

addRoundDown :: (IEEE a) => a -> a -> a
addRoundDown a b = predIEEE $ a + b

subRoundUp :: (IEEE a) => a -> a -> a
subRoundUp a b = succIEEE $ a - b

subRoundDown :: (IEEE a) => a -> a -> a
subRoundDown a b = predIEEE $ a - b

mulRoundUp :: (IEEE a) => a -> a -> a
mulRoundUp a b = succIEEE $ a * b

mulRoundDown :: (IEEE a) => a -> a -> a
mulRoundDown a b = predIEEE $ a * b

divRoundUp :: (IEEE a) => a -> a -> a
divRoundUp a b = succIEEE $ a / b

divRoundDown :: (IEEE a) => a -> a -> a
divRoundDown a b = predIEEE $ a / b

sqrtRoundUp :: (IEEE a) => a -> a
sqrtRoundUp = succIEEE . sqrt

sqrtRoundDown :: (IEEE a) => a -> a
sqrtRoundDown = predIEEE . sqrt

-- arguments are period, range, derivative, function, and interval
-- we require that each period of the function include precisely one local minimum and one local maximum
periodic :: (IEEE a) => a -> Interval a -> (a -> Ordering) -> (a -> a) -> Interval a -> Interval a
periodic p r _ _ x | width x > p = r
periodic _ r d f (I a b) = periodic' r (d a) (d b) (f a) (f b)

-- arguments are global range, derivatives at endpoints, values at endpoints
periodic' :: (IEEE a) => Interval a -> Ordering -> Ordering -> a -> a -> Interval a
periodic' r GT GT a b
  | a <= b = roundOut r a b -- stays in increasing zone
  | otherwise = r -- goes from increasing zone, all the way through decreasing zone, and back to increasing zone
periodic' r LT LT a b
  | a >= b = roundOut r b a -- stays in decreasing zone
  | otherwise = r -- goes from decreasing zone, all the way through increasing zone, and back to decreasing zone
periodic' r GT _ a b = I (min a b) (sup r) -- was going up, started going down
periodic' r LT _ a b = I (inf r) (max a b) -- was going down, started going up
periodic' r EQ GT a b
  | a < b = roundOut r a b -- stays in increasing zone
  | otherwise = r -- goes from increasing zone, all the way through decreasing zone, and back to increasing zone
periodic' r EQ LT a b
  | a > b = roundOut r b a -- stays in decreasing zone
  | otherwise = r -- goes from decreasing zone, all the way through increasing zone, and back to decreasing zone
periodic' _ _ _ a b = a ... b -- precisely begins and ends at local extremes, so it's either a singleton or whole

roundOut :: (IEEE a) => Interval a -> a -> a -> Interval a
roundOut (I lo hi) a b = I (max lo $ predIEEE a) (min hi $ succIEEE b)

signum' :: (Ord a, Num a) => a -> Ordering
signum' x = compare x 0
