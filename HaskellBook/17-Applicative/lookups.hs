module Lookups where

import Data.List (elemIndex)

--pure :: Applicative f => a -> f a
--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b

added :: Maybe Integer
added =
  (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

a :: Maybe Int
a = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> a

xs = [1, 2, 3]
ys = [4, 5, 6]

b :: Maybe Integer
b = lookup 3 $ zip xs ys

c :: Maybe Integer
c = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (<$>) sum $ (,) <$> b <*> c
