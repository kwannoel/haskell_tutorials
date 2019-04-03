{-# LANGUAGE InstanceSigs #-}

module Compre where

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure = \x -> Reader $ \_ -> x
-- runReader :: Reader r a -> (r -> a)
-- (<*>) :: f (a -> b) -> f a -> f b
-- contextualized:
-- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
-- going within: r -> a -> b -> (r -> a) -> (r -> b)
-- (<*>) (Reader rab) (Reader ra) = Reader $ rab <*> ra
  Reader rab <*> Reader ra =
    Reader $ \r -> rab r . ra $ r

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) r

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
  dogsName      :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Person -> Dog
  -- dogName :: Person -> DogName
  -- address :: Person -> Address
  -- Dog :: DogName -> Address -> Dog
  -- >>= :: m a -> (a -> m b) -> m b
  -- >>= :: Person -> DogName -> (DogName -> (Person -> Dog)) -> Person -> Dog
  -- Person -> Dog = Person -> DogName -> Address -> Dog
getDogRM = runReader $ Reader dogName >>= \dogName' -> (Reader $ Dog dogName' . address)
