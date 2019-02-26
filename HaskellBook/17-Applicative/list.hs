module Listapp where

import Test.QuickCheck.Checkers

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup a => Semigroup (List a) where
  (<>) Nil Nil = Nil
  (<>) Nil c@_ = c
  (<>) d@_ Nil = d
  (<>) (Cons a bs) (Cons x ys) =
    Cons ((<>) a x) $ (<>) bs ys

instance Monoid a => Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f as = concat' $ f <$> as

instance Applicative List where
  pure a = Cons a Nil
  -- f (a -> b) -> f a -> f b
  -- List (a -> b) -> List a -> List b
  -- List (a -> List b) -> List a -> List (List b)
  -- fmap :: (a -> b) -> (g a -> g b)
  -- . :: (b -> c) -> (a -> b) -> (a -> c)
  -- fmap . fmap :: (a -> b) -> f g a -> f g b
  -- fmap :: (g a -> g b) -> f (g a) -> f (g b)
  -- (<*>) :: List (a -> b) -> List a -> List b
  (<*>) Nil (Cons f fs) = Nil
  (<*>) (Cons f fs) Nil = Nil
  (<*>) conA conB = flatMap' (<$> conB) conA
  -- ($ conB) :: (List a -> List b) -> List b
  -- ConA :: (List (a -> b))
  -- someFunction
  -- fmap something ConA, change all native functions to fmap f
  -- conB :: List a
  -- conA :: List (a -> b)
  -- trnf :: ((a -> b) -> b) -> List (a -> b) -> List b
--  (<*>) (Cons f fs) con = concat' $ Cons (f <$> con) ((<*>) ((fmap . fmap) pure $ fs) con)
--                                List b   List b

take' :: Int -> List a -> List a
take' = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving(Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList'


















