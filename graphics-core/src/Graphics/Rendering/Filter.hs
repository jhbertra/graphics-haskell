module Graphics.Rendering.Filter where

import Linear
import Linear.Affine

class Filter f a | f -> a where
  filterRadius :: f -> V2 a
  evalFilter :: f -> Point V2 a -> a
  integrateFilter :: f -> a
  sampleFilter :: Point V2 a -> f -> FilterSample a

data FilterSample a = FilterSample
  { _fsValue :: a
  , _fsWeight :: a
  }
  deriving (Show, Read, Eq, Ord, Functor)
