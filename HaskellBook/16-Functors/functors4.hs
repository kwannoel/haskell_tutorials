-- fmap id -- id
-- fmap (f . g) == fmap f . fmap g
-- fmap :: (a -> b) -> f a -> f b

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter $ f a

-- (fmap . fmap) f a == fmap (fmap f) a
-- what fmap does is to go into the structure of a
-- hence taking a function which changes the contents of a keeping the overarching structure intact
-- and mapping that over the outer structure means the inner structure's inner contents are the
--ones being changed.



