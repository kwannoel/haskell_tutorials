module Monoids where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  (<>) Trivial Trivial = Trivial 

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity x =
  x <> mempty == x

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity x =
  mempty <> x == x



main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity

  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)
