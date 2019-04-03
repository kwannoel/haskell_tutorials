{-# LANGUAGE InstanceSigs #-}
module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

both :: [Char] -> ([Char], [Char])
both = (,) <$> cap <*> rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a, b)
  -- return :: a -> m a
  -- [a] -> ([a], ) [a]
    -- ([Char], [Char]) -> ([Char] -> ([Char], [Char]))

tupled' :: [Char] -> ([Char], [Char])
tupled' = \str -> (cap str, rev str)

tupled'' :: [Char] -> ([Char], [Char])
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- cap/rev :: (->) [Char] [Char]
  -- 2nd bind :: [Char] -> [Char]
           -- -> [Char] -> ([Char] -> ([Char], [Char]))
           -- -> [Char] -> ([Char], [Char]))
tupled'' =
  cap >>= \str -> rev >>= \str' -> return (str, str')

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id
