module Definitions where
{-
class (Functor t, Foldable t)
   => Traversable t where
    traverse :: Applicative f
             => (a -> f b)
             -> t a
             -> f (t b)
    traverse f = sequenceA . fmap f

    sequenceA :: Applicative f
              => t (f a)
              -> f (t a)
    fmap :: Functor f => (a -> b) -> f a -> f b

(sequence .) . fmap = \f ta -> sequence (fmap f ta)
sequence . (fmap f)
sequence (fmap f ta)

laws

Naturality

t . traverse f = traverse (t . f)

function composition behaves as expected

identity

traverse Identity = Identity

traverse cannot add or inject any struct or effects

-}
