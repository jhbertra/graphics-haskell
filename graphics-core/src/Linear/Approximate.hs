module Linear.Approximate where

import Linear
import Test.QuickCheck

(=~) :: (Epsilon a) => a -> a -> Bool
(=~) a b = nearZero $ a - b

(==~) :: (Epsilon a, Show a) => a -> a -> Property
(==~) a b = counterexample (show a ++ " /~ " ++ show b) $ a =~ b

(/~) :: (Epsilon a) => a -> a -> Bool
(/~) a b = not $ a =~ b

(/=~) :: (Epsilon a, Show a) => a -> a -> Property
(/=~) a b = counterexample (show a ++ " =~ " ++ show b) $ a /~ b

compareApproximate :: (Epsilon a, Ord a) => a -> a -> Ordering
compareApproximate a b = if nearZero d then EQ else compare a b
  where
    d = a - b

(<~) :: (Epsilon a, Ord a) => a -> a -> Bool
(<~) a b = case compareApproximate a b of
  LT -> True
  _ -> False

(>~) :: (Epsilon a, Ord a) => a -> a -> Bool
(>~) a b = case compareApproximate a b of
  GT -> True
  _ -> False

(<=~) :: (Epsilon a, Ord a) => a -> a -> Bool
(<=~) a b = case compareApproximate a b of
  LT -> True
  EQ -> True
  _ -> False

(>=~) :: (Epsilon a, Ord a) => a -> a -> Bool
(>=~) a b = case compareApproximate a b of
  GT -> True
  EQ -> True
  _ -> False
