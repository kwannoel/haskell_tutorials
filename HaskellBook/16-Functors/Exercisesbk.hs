-- 1) f :: a -> a
-- a is kind *

-- 2) f :: a -> b a -> T (b a)
-- b is kind *, T is kind * -> * -> *

-- 3) f :: c a b -> c b a
-- c is kind * -> * -> *

module Exercisesbk where

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ (FixMe) = FixMe
  fmap f (Pls a) = Pls (f a)

{-

instance Functor ((->) a) where
  fmap f g = f . g

for some function f, type signature is a -> b == (->) a b
to keep structure intact, i.e. (->) a
we can only modify b
and this can be done through function composition
f . g.
fmap :: (a -> b) -> f a -> f b
fmap :: (y -> z) -> (((->) x) y) -> ((-> x) z)
fmap :: (y -> z) -> (x -> y) -> (x -> z)
-}

