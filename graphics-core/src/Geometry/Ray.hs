{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Geometry.Ray where

import Control.Comonad (Comonad)
import Control.Comonad.Traced (Comonad (..))
import Control.Lens (Field2 (_2), Lens, Traversal', lens, makeLenses, makePrisms, over)
import GHC.Generics (Generic, Generic1)
import Geometry.Parametric (Parametric (..))
import Linear (V3, (^*))
import Linear.Affine (Affine (..), Point (..))
import Linear.Affine.Arbitrary ()
import Linear.Arbitrary ()
import Test.QuickCheck (Arbitrary, genericShrink, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data Ray a = Ray
  { _o :: {-# UNPACK #-} Point V3 Float
  , _d :: {-# UNPACK #-} V3 Float
  , _rayEnv :: a
  }
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

$(makeLenses ''Ray)

instance (Arbitrary a) => Arbitrary (Ray a) where
  arbitrary = Ray <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Comonad Ray where
  extract = _rayEnv
  {-# INLINE extract #-}
  duplicate r@Ray{..} = Ray{_rayEnv = r, ..}
  {-# INLINE duplicate #-}

instance Parametric (Ray a) V3 Float where
  pointAt t Ray{..} = _o .+^ _d ^* t
  {-# INLINE pointAt #-}

data RayWithDifferentials a
  = RayWithNoDifferentials {-# UNPACK #-} (Ray a)
  | RayWithDifferentials {-# UNPACK #-} (Ray a) {-# UNPACK #-} RayDifferentials
  deriving (Ord, Eq, Generic, Generic1, Functor, Show, Read)

instance (Arbitrary a) => Arbitrary (RayWithDifferentials a) where
  arbitrary =
    oneof
      [ RayWithNoDifferentials <$> arbitrary
      , RayWithDifferentials <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

rdRay :: Lens (RayWithDifferentials a) (RayWithDifferentials b) (Ray a) (Ray b)
rdRay = lens get set
  where
    set :: RayWithDifferentials a -> Ray b -> RayWithDifferentials b
    set rd r = case rd of
      RayWithNoDifferentials _ -> RayWithNoDifferentials r
      RayWithDifferentials _ ds -> RayWithDifferentials r ds
    {-# INLINE set #-}
    get :: RayWithDifferentials a -> Ray a
    get = \case
      RayWithNoDifferentials r -> r
      RayWithDifferentials r _ -> r
    {-# INLINE get #-}

instance Comonad RayWithDifferentials where
  extract (RayWithNoDifferentials r) = extract r
  extract (RayWithDifferentials r _) = extract r
  {-# INLINE extract #-}
  duplicate rd@(RayWithNoDifferentials Ray{..}) =
    RayWithNoDifferentials (Ray{_rayEnv = rd, ..})
  duplicate rd@(RayWithDifferentials Ray{..} ds) =
    RayWithDifferentials (Ray{_rayEnv = rd, ..}) ds
  {-# INLINE duplicate #-}

data RayDifferentials = RayDifferentials
  { _rxOrigin :: {-# UNPACK #-} V3 Float
  , _ryOrigin :: {-# UNPACK #-} V3 Float
  , _rxDirection :: {-# UNPACK #-} V3 Float
  , _ryDirection :: {-# UNPACK #-} V3 Float
  }
  deriving (Ord, Eq, Generic, Show, Read)

instance Arbitrary RayDifferentials where
  arbitrary =
    RayDifferentials
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

$(makePrisms ''RayWithDifferentials)
$(makeLenses ''RayDifferentials)

rdDifferentials :: Traversal' (RayWithDifferentials a) RayDifferentials
rdDifferentials = _RayWithDifferentials . _2

scaleDifferentials :: Float -> RayWithDifferentials a -> RayWithDifferentials a
scaleDifferentials s = over rdDifferentials \RayDifferentials{..} ->
  RayDifferentials
    { _rxOrigin = _rxOrigin ^* s
    , _ryOrigin = _ryOrigin ^* s
    , _rxDirection = _rxDirection ^* s
    , _ryDirection = _ryDirection ^* s
    }
{-# INLINE scaleDifferentials #-}

class IsRay r where
  ray :: Lens (r a) (r b) (Ray a) (Ray b)

instance IsRay Ray where
  ray = id

instance IsRay RayWithDifferentials where
  ray = rdRay
