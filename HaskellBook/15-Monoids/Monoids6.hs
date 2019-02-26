module Monoids6 where

newtype Combine a b =
  Combine { unCombine :: (a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f1) (Combine f2) = (Combine (\n -> f1 n <> f2 n))

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)
