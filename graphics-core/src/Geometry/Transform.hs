module Geometry.Transform where

import Control.Lens (Lens, foldMapOf, lens, (.~), (^.))
import Data.Function (on, (&))
import GHC.Show (showSpace)
import Geometry.Bounds (Bounds)
import qualified Geometry.Bounds as Bounds
import Geometry.Normal (Normal (..))
import Geometry.Ray (IsRay (..), Ray (..))
import Linear (
  Epsilon,
  M44,
  Metric (quadrance),
  Quaternion,
  R3 (_xyz),
  V1 (..),
  V3 (..),
  V4 (V4),
  axisAngle,
  cross,
  det33,
  det44,
  dot,
  identity,
  inv44,
  mkTransformation,
  normalize,
  transpose,
  (!*),
  (!*!),
  (*!!),
  (*^),
  (^*),
  (^/),
 )
import Linear.Affine (Affine (..), Point (..), unP)
import qualified Linear.Projection as P
import Numeric.IEEE (IEEE (copySign))
import qualified Numeric.Interval.IEEE as I
import Test.QuickCheck hiding (scale)
import Text.Read (Lexeme (..), Read (..), lexP, parens, prec)

data Transform a = UnsafeTransform
  { _transformM :: M44 a
  , _transformMInv :: M44 a
  }
  deriving (Functor)

instance (Eq a) => Eq (Transform a) where
  a == b = on (==) _transformM a b
  a /= b = on (/=) _transformM a b

instance (Ord a) => Ord (Transform a) where
  compare = on compare _transformM

instance (Show a) => Show (Transform a) where
  showsPrec p UnsafeTransform{..} =
    showParen
      (p > 10)
      (showString "transform" . showSpace . showsPrec 11 _transformM)

instance (Fractional a, Read a) => Read (Transform a) where
  readPrec = parens $ prec 10 do
    Ident "transform" <- lexP
    transform <$> readPrec

transform :: (Fractional a) => M44 a -> Transform a
transform _transformM =
  UnsafeTransform
    { _transformM
    , _transformMInv = inv44 _transformM
    }

class ApplyTransform f a where
  (!!*!!) :: Transform a -> f a -> f a

infixl 7 !!*!!

instance (Num a) => ApplyTransform Transform a where
  UnsafeTransform m mInv !!*!! UnsafeTransform m' mInv' =
    UnsafeTransform (m !*! m') (mInv' !*! mInv)

instance (Eq a, Fractional a) => ApplyTransform (Point V3) a where
  UnsafeTransform{..} !!*!! (P (V3 x y z))
    | w == 1 = P v
    | otherwise = P $ v ^/ w
    where
      v = V3 x' y' z'
      V4 x' y' z' w = _transformM !* V4 x y z 1

instance (Num a) => ApplyTransform V3 a where
  UnsafeTransform{..} !!*!! v = fmap (^. _xyz) (_transformM ^. _xyz) !* v

instance (Floating a, Epsilon a) => ApplyTransform (Normal V3) a where
  UnsafeTransform{..} !!*!! (N v) =
    normalize $ N $ fmap (^. _xyz) (transpose _transformMInv ^. _xyz) !* v

instance (IEEE a) => ApplyTransform Ray a where
  (!!*!!) t = snd . transformRay t 0

transformRay :: (IEEE a) => (IsRay r) => Transform a -> a -> r a -> (a, r a)
transformRay t tMax r = (tMax', r & ray .~ r')
  where
    Ray{..} = r ^. ray
    o = (I.singleton <$> t) !!*!! (I.singleton <$> _o)
    d = t !!*!! _d
    lSquared = quadrance d
    (tMax', o')
      | lSquared == 0 = (tMax, o)
      | otherwise =
          let dt = dot (abs d) (pointMargin o) / lSquared
              d' = I.singleton <$> d ^* dt
           in (tMax - dt, o .+^ d')
    r' = Ray (I.midpoint <$> o') d _time

instance (Ord a, Bounded a, Fractional a) => ApplyTransform (Bounds V3) a where
  (!!*!!) t = foldMapOf Bounds.corners3 (Bounds.singularity . (t !!*!!))

pointMargin :: (Functor f, Fractional a) => Point f (I.Interval a) -> f a
pointMargin = fmap I.margin . unP

transformM :: (Fractional b) => Lens (Transform a) (Transform b) (M44 a) (M44 b)
transformM = lens _transformM $ const transform

transformMInv :: (Fractional b) => Lens (Transform a) (Transform b) (M44 a) (M44 b)
transformMInv = lens _transformMInv $ const $ invTransform . transform

invTransform :: Transform a -> Transform a
invTransform (UnsafeTransform m mInv) = UnsafeTransform mInv m

detTransform :: (Num a) => Transform a -> a
detTransform (UnsafeTransform m _) = det44 m

transposeTransform :: Transform a -> Transform a
transposeTransform (UnsafeTransform m mInv) =
  UnsafeTransform (transpose m) (transpose mInv)

isIdentity :: (Eq a, Num a) => Transform a -> Bool
isIdentity = (== identity) . _transformM

instance (Arbitrary a, Floating a, Epsilon a) => Arbitrary (Transform a) where
  arbitrary = sized \case
    0 -> oneof leaves
    n -> oneof $ resize (n `div` 2) ((!!*!!) <$> arbitrary <*> arbitrary) : leaves
    where
      leaves =
        [ translate <$> arbitrary
        , scale <$> arbitrary
        , rotate <$> arbitrary <*> arbitrary
        ]

translate :: (Num a) => V3 a -> Transform a
translate (V3 x y z) =
  UnsafeTransform
    ( V4
        (V4 1 0 0 x)
        (V4 0 1 0 y)
        (V4 0 0 1 z)
        (V4 0 0 0 1)
    )
    ( V4
        (V4 1 0 0 $ -x)
        (V4 0 1 0 $ -y)
        (V4 0 0 1 $ -z)
        (V4 0 0 0 1)
    )

scale :: (Fractional a) => V3 a -> Transform a
scale (V3 x y z) =
  UnsafeTransform
    ( V4
        (V4 x 0 0 0)
        (V4 0 y 0 0)
        (V4 0 0 z 0)
        (V4 0 0 0 1)
    )
    ( V4
        (V4 (recip x) 0 0 0)
        (V4 0 (recip y) 0 0)
        (V4 0 0 (recip z) 0)
        (V4 0 0 0 1)
    )

rotateX :: (Floating a) => a -> Transform a
rotateX theta =
  UnsafeTransform m $ transpose m
  where
    cosTheta = cos theta
    sinTheta = sin theta
    m =
      V4
        (V4 1 0 0 0)
        (V4 0 cosTheta (-sinTheta) 0)
        (V4 0 sinTheta cosTheta 0)
        (V4 0 0 0 1)

rotateY :: (Floating a) => a -> Transform a
rotateY theta =
  UnsafeTransform m $ transpose m
  where
    cosTheta = cos theta
    sinTheta = sin theta
    m =
      V4
        (V4 cosTheta 0 sinTheta 0)
        (V4 0 1 0 0)
        (V4 (-sinTheta) 0 cosTheta 0)
        (V4 0 0 0 1)

rotateZ :: (Floating a) => a -> Transform a
rotateZ theta =
  UnsafeTransform m $ transpose m
  where
    cosTheta = cos theta
    sinTheta = sin theta
    m =
      V4
        (V4 cosTheta (-sinTheta) 0 0)
        (V4 sinTheta cosTheta 0 0)
        (V4 0 0 1 0)
        (V4 0 0 0 1)

rotate' :: (Floating a, Epsilon a) => V3 a -> a -> a -> Transform a
rotate' (normalize -> V3 x y z) cosTheta sinTheta =
  UnsafeTransform m $ transpose m
  where
    xx = x * x
    xy = x * y
    xz = x * z
    yy = y * y
    yz = y * z
    zz = z * z
    xSinTheta = x * sinTheta
    ySinTheta = y * sinTheta
    zSinTheta = z * sinTheta
    m =
      V4
        ( V4
            (xx + (1 - xx) * cosTheta)
            (xy * (1 - cosTheta) - zSinTheta)
            (xz * (1 - cosTheta) + ySinTheta)
            0
        )
        ( V4
            (xy * (1 - cosTheta) + zSinTheta)
            (yy + (1 - yy) * cosTheta)
            (yz * (1 - cosTheta) - xSinTheta)
            0
        )
        ( V4
            (xz * (1 - cosTheta) - ySinTheta)
            (yz * (1 - cosTheta) + xSinTheta)
            (zz + (1 - zz) * cosTheta)
            0
        )
        (V4 0 0 0 1)

rotate :: (Floating a, Epsilon a) => V3 a -> a -> Transform a
rotate axis theta = fromQuaternion $ axisAngle axis theta

fromQuaternion :: (Floating a) => Quaternion a -> Transform a
fromQuaternion q =
  UnsafeTransform m $ transpose m
  where
    m = mkTransformation q 0

rotateFromTo :: forall a. (Floating a, Epsilon a, Ord a) => V3 a -> V3 a -> Transform a
rotateFromTo (normalize -> f@(V3 fromX fromY _)) (normalize -> t@(V3 toX toY _)) =
  UnsafeTransform m $ transpose m
  where
    h v@(V3 i j k) = identity - (2 / dot v v) *!! (v' !*! transpose v')
      where
        v' = V4 (V1 i) (V1 j) (V1 k) 0
    m = h (refl - t) !*! h (refl - f)
    refl
      | abs fromX < 0.72 && abs toX < 0.72 = V3 1 0 0
      | abs fromY < 0.72 && abs toY < 0.72 = V3 0 1 0
      | otherwise = V3 0 0 1

lookAt :: (Epsilon a, Floating a) => Point V3 a -> Point V3 a -> V3 a -> Transform a
lookAt (P pos) (P focus) up = transform $ P.lookAt pos focus up

perspective :: (Floating a) => a -> a -> a -> a -> Transform a
perspective fov aspect near far =
  UnsafeTransform
    (P.perspective fov aspect near far)
    (P.inversePerspective fov aspect near far)

infinitePerspective :: (Floating a) => a -> a -> a -> Transform a
infinitePerspective fov aspect near =
  UnsafeTransform
    (P.infinitePerspective fov aspect near)
    (P.inverseInfinitePerspective fov aspect near)

frustum :: (Floating a) => a -> a -> a -> a -> a -> a -> Transform a
frustum left right bottom top near far =
  UnsafeTransform
    (P.frustum left right bottom top near far)
    (P.inverseFrustum left right bottom top near far)

ortho :: (Floating a) => a -> a -> a -> a -> a -> a -> Transform a
ortho left right bottom top near far =
  UnsafeTransform
    (P.ortho left right bottom top near far)
    (P.inverseOrtho left right bottom top near far)

swapsHandedness :: (Num a, Ord a) => Transform a -> Bool
swapsHandedness (UnsafeTransform m _) = det33 (fmap (^. _xyz) (m ^. _xyz)) < 0

data Frame a = Frame
  { _frameX :: V3 a
  , _frameY :: V3 a
  , _frameZ :: V3 a
  }
  deriving (Show, Read, Eq, Ord, Functor)

frameFromXZ :: (Num a) => V3 a -> V3 a -> Frame a
frameFromXZ x z = Frame x (cross z x) z

frameFromXY :: (Num a) => V3 a -> V3 a -> Frame a
frameFromXY x y = Frame x y (cross x y)

frameFromX :: (IEEE a) => V3 a -> Frame a
frameFromX x = case coordinateSystem x of
  (y, z) -> Frame x y z

frameFromY :: (IEEE a) => V3 a -> Frame a
frameFromY y = case coordinateSystem y of
  (x, z) -> Frame x y z

frameFromZ :: (IEEE a) => V3 a -> Frame a
frameFromZ z = case coordinateSystem z of
  (x, y) -> Frame x y z

toLocal :: (Num a) => V3 a -> Frame a -> V3 a
toLocal v Frame{..} =
  V3
    (dot v _frameX)
    (dot v _frameY)
    (dot v _frameZ)

fromLocal :: (Num a) => V3 a -> Frame a -> V3 a
fromLocal (V3 x y z) Frame{..} = x *^ _frameX + y *^ _frameY + z *^ _frameZ

coordinateSystem :: (IEEE a) => V3 a -> (V3 a, V3 a)
coordinateSystem (V3 x y z) = (v2, v3)
  where
    sign = copySign 1 z
    a = -1 / sign + z
    b = x * y * a
    v2 = V3 (1 + sign + x * x * a) (sign * b) (-sign * x)
    v3 = V3 b (sign + y * y * a) (-y)
