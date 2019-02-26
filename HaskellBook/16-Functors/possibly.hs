module Possibly where

-- fmap id == id
-- fmap f . fmap g = fmap $ f . g

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b)  = Second $ f b

newtype Constant' a b =
  Constant' { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' x) = Constant' x

data Wrap f b =
  Wrap (f b)
  deriving (Eq, Show)



instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fx) = Wrap $ fmap f fx

-- since fx is f x, it has to have a functor defined
-- thus we use fmap to get into its structure
