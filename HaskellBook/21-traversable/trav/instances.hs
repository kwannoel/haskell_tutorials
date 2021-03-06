module Tinstances where

data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  pure a = Right' a
  Left' a <*> _ = Left' a
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' x) = Right' <$> f x

{-

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Monoid a
         => Applicative ((,) a) where
       pure x = (mempty, x)
       (u, f) <*> (v, x) =
         (u `mappend` v, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable ((,) a) where
  traverse f (x, y) = ((,) x <$> f y)

-}
