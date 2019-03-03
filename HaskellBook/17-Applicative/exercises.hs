module Exercises where

import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes

{-
[]
pure :: a -> ? a
(<*>) :: [] (a -> b) -> ? a -> ? b

IO
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

(,) a
pure :: a -> (,) b a
(<*>) :: (,) c (a -> b) -> (,) c a -> (,) c b

(->) e
pure :: a -> ((->) e a)
(<*>) :: ? (a -> b) -> ? a -> ? b


-}

main :: IO ()
main = do
  quickBatch (
    applicative (
      Pair (undefined,
            undefined,
            undefined)
           (undefined,
            undefined,
            undefined) ::
        Pair (Int,
              String,
              Integer)))
  quickBatch (
    applicative (
      Two (undefined, undefined, undefined) (undefined, undefined, undefined)
   :: Two (String, String, String) (String, String, String)))

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f1) (Pair a b) = Pair (f1 a) (f1 b)

instance Eq a => EqProp (Pair a) where
  (=-=) (Pair a b) (Pair x y) = eq a b .&. eq x y

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)


data Two a b = Two a b deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f2) (Two x y) = Two (a <> x) (f2 y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  -- property :: prop -> Property
  -- eq :: a -> a -> Property
  (=-=) (Two a b) (Two c d) = property (a == c && b == d)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b c) (Three x y z) = Three (a <> x) (b <> y) (c z)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z $ f b

instance Monoid a => Applicative (Four' a) where
  pure c = Four' mempty mempty mempty c
  (<*>) (Four' a b c d) (Four' w x y z) = Four' (a <> w) (b <> x) (c <> y) (d z)
