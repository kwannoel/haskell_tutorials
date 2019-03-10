-- right identity
-- m >>= return = m

-- left identity
-- return x >>= f = f x

-- (>>=) :: Monad m
--       => m a -> (a -> m b) -> m b

-- return :: a -> m a

-- Associativity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

