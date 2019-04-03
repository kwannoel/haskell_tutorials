module Exercises where

import Data.Monoid

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldl f z (Constant b) = f z b
  foldMap f (Constant b) = f b

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldl f z (Two a b) = f z b
  foldMap f (Two a b) = f b

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z
  foldl f z (Three a b c) = f z c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' a b b1) = b `f` (b1 `f` z)
  foldl f z (Three' a b b1) = (z `f` b1) `f` b
  foldMap f (Three' a b b1) = f b <> f b1

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' a b b1 b2) = f b . f b1 . f b2 $ z
  foldl f z (Four' a b b1 b2) = f z b `f` b1 `f` b2
  foldMap f (Four' a b b1 b2) = f b <> f b1 <> f b2

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f foldable =
  foldMap filterF' foldable where
    filterF' x = if f x then pure x else mempty
