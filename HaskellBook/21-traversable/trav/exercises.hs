{-# LANGUAGE FlexibleContexts #-}
module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid (Sum)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = a `f` z
  foldl f z (Identity a) = z `f` a

instance Traversable Identity where
  -- traverse :: (Functor f, Foldable t) => (a -> f b) -> t a -> f (t a)
  traverse f (Identity a) = Identity <$> f a
  -- sequence :: (Functor f, Foldable t) => t (f a) -> f (t a)
  sequence (Identity x) = Identity <$> x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant f <*> Constant a = Constant $ f <> a

instance Foldable (Constant a) where
  foldMap _ _ = mempty
  foldr _ z _ = z
  foldl _ z _ = z

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a
  sequence (Constant a) = pure $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

data Optional a =
    Nada
  | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
  foldr f z Nada = z
  foldr f z (Yep a) = f a z
  foldl f z Nada = z
  foldl f z (Yep a) = f z a

instance Traversable Optional where
  -- traverse :: (a -> f b) -> t a -> f (t b)
  -- sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a
  sequenceA Nada = pure Nada
  sequenceA (Yep a) = Yep <$> a

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof $ return <$> [Yep a, Nada]

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = f a `Cons` fmap f ls

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons a ls) = f a <> foldMap f ls
  foldr f z Nil = z
  foldr f z (Cons a ls) = a `f` foldr f z ls
  foldl f z Nil = z
  foldl f z (Cons a ls) = foldl f (z `f` a) ls

instance Traversable List where
  traverse f Nil = pure Nil
  -- Cons :: a -> List a -> List a
  -- f :: a -> f a
  -- Cons <$> (f a) :: f (List a -> List a)
  -- traverse
  traverse f (Cons a b) = Cons <$> (f a) <*> traverse f b
  sequenceA Nil = pure Nil
  sequenceA (Cons a ls) = Cons <$> a <*> sequenceA ls

instance Eq a => EqProp (List a) where
  (=-=) = eq

toList :: [] a -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = toList <$> listOf arbitrary

data Three a b c =
  Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z
  foldl f z (Three a b c) = f z c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
  sequenceA (Three a b fc) = fmap (Three a b) fc

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Pair a b =
  Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b
  foldr f z (Pair a b) = f b z
  foldl f z (Pair a b) = f z b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b
  sequenceA (Pair a b) = Pair a <$> b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b1) = Big a (f b) (f b1)

instance Foldable (Big a) where
  foldMap f (Big a b b1) = f b <> f b1
  foldr f z (Big a b b1) = b `f` (b1 `f` z)
  foldl f z (Big a b b1) = z `f` b `f` b1

instance Traversable (Big a) where
  traverse f (Big a b b1) = Big a <$> f b <*> f b1
  sequenceA (Big a fb fb1) = Big a <$> fb <*> fb1

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq
  -- (S x y) =-= (S p q) =
  --       (property $ (=-=) <$> x <*> p)
  --   .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  -- foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S na a) = foldMap f na <> f a
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z (S na a) = f a $ foldr f z na
  foldl f z (S na a) = f (foldl f z na) a

instance Traversable n
      => Traversable (S n) where
  -- traverse :: (a -> f a) -> S n a -> f (S n a)
  -- S :: n a -> a -> S n a
  -- S (na) :: a -> S n a
  traverse f (S na a) = S <$> (traverse f na) <*> f a
  sequenceA (S nfa fa) = S <$> (sequenceA nfa) <*> fa

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node ta a ta2) = Node (fmap f ta) (f a) (fmap f ta2)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ta a ta2) = foldMap f ta <> f a <> foldMap f ta2
  foldr f z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node ta a ta2) = foldr f (a `f` foldr f z ta2) ta

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node ta a ta2) = Node <$> traverse f ta <*> f a <*> traverse f ta2
  sequenceA = traverse id

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (String, String, String))
  quickBatch $ traversable (undefined :: Constant String (String, String, String))
  quickBatch $ traversable (undefined :: Optional (String, String, String))
  quickBatch $ traversable (undefined :: List (String, String, String))
  quickBatch $ traversable (undefined :: Three String String (String, String, String))
  quickBatch $ traversable (undefined :: S [] (String, String, String))

-- vs --

{-
  let samp :: (String, String, String)
      samp = undefined
  let wrapArr = (Identity, Constant String)
  (quickBatch . traversable . $ samp) `tupleSeqMap` wrapArr


-}
  {-
main :: IO ()
main = do
  let samp :: (String, String, String)
      samp = undefined
  let testables = (Identity, (Constant String))

-}
