module EitherM where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = 

instance Applicative (Sum a) where
  pure = undefined
  (<*>) = undefined

instance Monad (Sum a) where
  return = pure
  (>>=) = undefined
