{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Geometry.Ray where

import Control.Lens (Field2 (_2), Lens', Traversal', lens, makeLenses, makePrisms, over)
import GHC.Generics (Generic, Generic1)
import Geometry.Parametric (Parametric (..))
import Linear (V3, (^*))
import Linear.Affine (Affine (..), Point (..))
import Linear.Affine.Arbitrary ()
import Linear.Arbitrary ()
import Test.QuickCheck (Arbitrary, genericShrink, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data Ray a = Ray
  { _o :: Point V3 a
  , _d :: V3 a
  , _time :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

$(makeLenses ''Ray)

instance (Arbitrary a) => Arbitrary (Ray a) where
  arbitrary = Ray <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Num a) => Parametric (Ray a) V3 a where
  pointAt t Ray{..} = _o .+^ _d ^* t
  {-# INLINE pointAt #-}

data RayWithDifferentials a
  = RayWithNoDifferentials (Ray a)
  | RayWithDifferentials (Ray a) (RayDifferentials a)
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

instance (Arbitrary a) => Arbitrary (RayWithDifferentials a) where
  arbitrary =
    oneof
      [ RayWithNoDifferentials <$> arbitrary
      , RayWithDifferentials <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

rdRay :: Lens' (RayWithDifferentials a) (Ray a)
rdRay = lens get set
  where
    set :: RayWithDifferentials a -> Ray a -> RayWithDifferentials a
    set rd r = case rd of
      RayWithNoDifferentials _ -> RayWithNoDifferentials r
      RayWithDifferentials _ ds -> RayWithDifferentials r ds
    {-# INLINE set #-}
    get :: RayWithDifferentials a -> Ray a
    get = \case
      RayWithNoDifferentials r -> r
      RayWithDifferentials r _ -> r
    {-# INLINE get #-}

data RayDifferentials a = RayDifferentials
  { _rxOrigin :: V3 a
  , _ryOrigin :: V3 a
  , _rxDirection :: V3 a
  , _ryDirection :: V3 a
  }
  deriving (Ord, Eq, Generic, Show, Read, Functor)

instance (Arbitrary a) => Arbitrary (RayDifferentials a) where
  arbitrary =
    RayDifferentials
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

$(makePrisms ''RayWithDifferentials)
$(makeLenses ''RayDifferentials)

rdDifferentials :: Traversal' (RayWithDifferentials a) (RayDifferentials a)
rdDifferentials = _RayWithDifferentials . _2

scaleDifferentials :: (Num a) => a -> RayWithDifferentials a -> RayWithDifferentials a
scaleDifferentials s = over rdDifferentials \RayDifferentials{..} ->
  RayDifferentials
    { _rxOrigin = _rxOrigin ^* s
    , _ryOrigin = _ryOrigin ^* s
    , _rxDirection = _rxDirection ^* s
    , _ryDirection = _ryDirection ^* s
    }
{-# INLINE scaleDifferentials #-}

class IsRay r where
  ray :: Lens' (r a) (Ray a)

instance IsRay Ray where
  ray = id

instance IsRay RayWithDifferentials where
  ray = rdRay
