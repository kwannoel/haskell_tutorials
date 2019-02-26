module Monoids2 where

import Test.QuickCheck

newtype Identity a =
  Identity a deriving Show

genId :: Arbitrary a => Gen (Identity a)
genId = do
  a <- arbitrary
  return $ Identity a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = genId

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  

