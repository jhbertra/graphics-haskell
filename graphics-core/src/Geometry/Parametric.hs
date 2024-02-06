{-# LANGUAGE FunctionalDependencies #-}

module Geometry.Parametric where

import Linear.Affine (Affine, Point)

class (Affine f, Num a) => Parametric s f a | s -> f a where
  pointAt :: a -> s -> Point f a
