module Exercises where

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Maybe' a =
  Just' a | Nothing'

instance Foldable Maybe' where
  foldr _ z Nothing' = z
  foldr f z (Just' a) = f a z
  foldl _ z Nothing' = z
  foldl f z (Just' a) = f z a
  foldMap _ Nothing' = mempty
  foldMap f (Just' a) = f a

