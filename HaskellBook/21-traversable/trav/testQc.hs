module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
-- (a -> b) -> f a -> f b
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
-- m a -> (a -> m b) -> m b
  _ >>= _ = NopeDotJpg
-- >>
-- >>=

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

main :: IO ()
main = do
  let nope :: Nope (Int, String, Integer)
      nope = undefined
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope

data PhhhbbtttEither b a =
    Left' a
  | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap f (Right' a) = Right' a

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' a <*> _ = Right' a
  _ <*> Right' a = Right' a
  Left' f <*> Left' b = Left' $ f b

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  Right' a >>= f = Right' a
-- f won't be Right' since Right' does not type check i.e. PhhhbbtttEither b _
-- (>>=) :: m a -> (a -> m b) -> m b
  Left' a >>= f = f a

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    oneof [return $ Left' a, return $ Right' b]

main2 :: IO ()
main2 = do
  let ether :: PhhhbbtttEither String (Integer, Int, String)
      ether = undefined
  quickBatch $ functor ether
  quickBatch $ applicative ether
  quickBatch $ monad ether

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
-- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (Identity a) f = f a
-- (>>) :: m a -> m b -> m b
-- m >> k = m >>= \_ -> k
  (>>) (Identity a) (Identity b) = Identity b

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary =
    arbitrary >>= return . Identity
  -- arbitrary :: Gen (Identity a)
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- return :: a -> m a
  -- In the case of arbitrary >>= return
  -- (>>=) :: <insert arb type> -> <insert return type> -> Gen (Identity a)
  -- deducing..
  -- (>>=) :: Gen (Identity a) -> (Identity a -> Gen(Identity a)) -> Gen (Identity a)
  -- This results in a recursive call to arbitrary as seen by the first and resultant types.
  -- Hence it is invalid

main3 :: IO ()
main3 =
  let ident :: Identity (Integer, Int, String)
      ident = undefined
  in quickBatch (functor ident)
  >> quickBatch (applicative ident)
  >> quickBatch (monad ident)

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a bs) = Cons (f a) $ fmap f bs

instance Applicative List where
  pure = flip Cons Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (list') =
    (f <$> list') `connect` (fs <*> list') where
    connect :: List a -> List a -> List a
    connect Nil l@_ = l
    connect l@_ Nil = l
    connect (Cons a bs) (Cons c ds) = Cons a $ bs `connect` Cons c ds

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = Cons y $ xs >>= f where
    Cons y Nil = f x

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    arbitrary >>= return

instance Eq a => Eq (List a) where
  Nil == Nil = True
  Nil == _ = False
  _ == Nil = False
  Cons x xs == Cons y ys = x == y && xs == ys

instance Show a => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = show x ++ show xs

instance Eq a => EqProp (List a) where
  (=-=) = eq

main4 :: IO ()
main4 =
  let ls :: List (String, Int, Integer)
      ls = undefined
  in  quickBatch (functor ls)
  >>  quickBatch (applicative ls)
  >>  quickBatch (monad ls)
