module Compose where

mcomp :: Monad m =>
         (b -> m c)
      -> (a -> m b)
      -> a
      -> m c
mcomp f g a = (g a) >>= f
