module EitherM where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure x = Second x
-- ap :: Sum c (a -> b) -> Sum c a -> Sum c b
  (<*>) _ (First a) = First a
  (<*>) (First f) _ = First f
  (<*>) (Second f) (Second a) = Second $ f a

instance Monad (Sum a) where
  return = pure
-- bind :: Sum c a -> (a -> Sum c b) -> Sum c b
  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a

