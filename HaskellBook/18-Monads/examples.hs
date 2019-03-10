module Example where

import Control.Applicative ((*>))

import Control.Monad

-- class Applicative m => Monad m where
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a
-- Functor -> Applicative -> Monad

bind :: Monad m => (a -> m b) -> m a -> m b
-- fmap :: (a -> b) -> f a -> f b
-- fmapInContext :: (a -> m b) -> m a -> m (m b)
-- join :: m (m a) -> m a
bind f = join . fmap f

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn "age pls:" >>
    getLine >>=
    \age ->
      putStrLn ("y helo that: "
                ++ name ++ " who is: "
                ++ age ++ " years old.")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>=
  \x ->
    if even x
    then [x*x, x*x]
    else []

