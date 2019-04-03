module LibFuncs where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' foldable = getSum $ foldMap Sum foldable

product' :: (Foldable t, Num a) => t a -> a
product' foldable = getProduct $ foldMap Product foldable

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a foldableA = getAny $ foldMap (Any . (a ==)) foldableA

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' foldable = foldr (justMin . Just) Nothing foldable
  where justMin x y = if y == Nothing then x else min x y

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' foldable = getFirst $ foldMap (First . Just) foldable

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' foldable = foldr (justMax . Just) Nothing foldable
  where justMax x y = if y == Nothing then x else max x y

maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' foldable = getLast $ foldMap (Last . Just) foldable

null' :: (Foldable t) => t a -> Bool
null' foldable = getAll $ foldMap (\_ -> All False) foldable

null'' :: (Foldable t) => t a -> Bool
null'' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x y -> x : y) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' foldable = foldMap id foldable
-- 1316

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> f x <> y) mempty
