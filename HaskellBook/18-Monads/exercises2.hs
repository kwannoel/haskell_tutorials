-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
j :: Monad m => m (m a) -> m a
j m 
