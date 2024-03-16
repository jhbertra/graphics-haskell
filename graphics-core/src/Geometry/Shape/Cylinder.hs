module Graphics.Shape.Cylinder (
  Cylinder,
  _cylinderFromRender,
  _cylinderTransformSwapsHandedness,
  _cylinderFlipNormals,
  _cylinderPhiMax,
  _cylinderRadius,
  _cylinderToRender,
  _cylinderHeight,
  cylinder,
  cylinderFlipNormals,
  cylinderPhiMax,
  cylinderRadius,
  cylinderHeight,
) where

import Control.Lens (Lens', lens)
import Data.Function (on)
import Data.Ord (clamp)
import GHC.Show (showSpace)
import Geometry.Transform
import Text.Read (Lexeme (..), Read (..), lexP, parens, prec)

data Cylinder a = Cylinder
  { _cylinderRadius :: a
  , _cylinderHeight :: a
  , _cylinderPhiMax :: a
  , _cylinderFlipNormals :: Bool
  , _cylinderFromRender :: Transform a
  , _cylinderToRender :: Transform a
  , _cylinderTransformSwapsHandedness :: Bool
  }

cylinder
  :: (Floating a, Ord a)
  => Transform a
  -> Transform a
  -> Bool
  -> a
  -> a
  -> a
  -> Cylinder a
cylinder _cylinderToRender _cylinderFromRender _cylinderFlipNormals (abs -> radius) _cylinderHeight phiMax =
  Cylinder
    { _cylinderRadius = radius
    , _cylinderHeight
    , _cylinderPhiMax = clamp (0, 2 * pi) phiMax
    , _cylinderFlipNormals
    , _cylinderFromRender
    , _cylinderToRender
    , _cylinderTransformSwapsHandedness = swapsHandedness _cylinderToRender
    }

cylinderRadius :: Lens' (Cylinder a) a
cylinderRadius = lens _cylinderRadius \c _cylinderRadius -> c{_cylinderRadius}

cylinderHeight :: Lens' (Cylinder a) a
cylinderHeight = lens _cylinderHeight \c _cylinderHeight -> c{_cylinderHeight}

cylinderPhiMax :: (Floating a, Ord a) => Lens' (Cylinder a) a
cylinderPhiMax = lens _cylinderPhiMax \s phiMax ->
  s{_cylinderPhiMax = clamp (0, 2 * pi) phiMax}

cylinderFlipNormals :: Lens' (Cylinder a) Bool
cylinderFlipNormals = lens _cylinderFlipNormals \s _cylinderFlipNormals -> s{_cylinderFlipNormals}

instance (Eq a) => Eq (Cylinder a) where
  a == b =
    on (==) _cylinderRadius a b
      && on (==) _cylinderHeight a b
      && on (==) _cylinderPhiMax a b
      && on (==) _cylinderFlipNormals a b
  a /= b =
    on (/=) _cylinderRadius a b
      || on (/=) _cylinderHeight a b
      || on (/=) _cylinderPhiMax a b
      || on (/=) _cylinderFlipNormals a b

instance (Ord a) => Ord (Cylinder a) where
  compare a b =
    on compare _cylinderRadius a b
      <> on compare _cylinderHeight a b
      <> on compare _cylinderPhiMax a b
      <> on compare _cylinderFlipNormals a b

instance (Show a) => Show (Cylinder a) where
  showsPrec p Cylinder{..} =
    showParen
      (p > 10)
      ( showString "cylinder"
          . showSpace
          . showsPrec 11 _cylinderToRender
          . showSpace
          . showsPrec 11 _cylinderFromRender
          . showSpace
          . showsPrec 11 _cylinderFlipNormals
          . showSpace
          . showsPrec 11 _cylinderRadius
          . showSpace
          . showsPrec 11 _cylinderHeight
          . showSpace
          . showsPrec 11 _cylinderPhiMax
      )

instance (Floating a, Read a, Ord a) => Read (Cylinder a) where
  readPrec = parens $ prec 10 do
    Ident "cylinder" <- lexP
    cylinder <$> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec <*> readPrec
