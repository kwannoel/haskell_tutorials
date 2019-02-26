module IOFuncts where

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

type Nat f g a = f a -> g a
maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)
-- this works, goofy as it looks.
instance Functor (Flip f a) where
  fmap f (Flip (f1 b a)) =
    Flip $ f1 (f b) a
