module Exercises2 where
-- fmap :: (a -> b) -> f a -> f b
-- join :: m (m a) -> m a
-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
-- (<*>) :: f (a -> b) -> f a -> f b
j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = \f y -> y >>= (pure . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] aMb = pure []
  -- (:) :: b -> [b] -> [b]
  -- arguments :: m b , m [b]
  -- 
  -- fmap (:) :: m b -> m ([b] -> [b])
  -- fmap . fmap $ (:) :: m b -> m
  -- (++) :: [a] -> [a] -> [a]
  -- m a -> (a -> m b) -> m b
  -- m b -> m [b] -> m [b]
meh (a:as) aMb = fmap (:) (pure a >>= aMb) <*> (meh as aMb)

flipType :: (Monad m) => [m a] -> m [a]
  -- meh :: [m a] -> (m a -> m a) -> m [a]
flipType ma = meh ma id
