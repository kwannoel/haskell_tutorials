module Gens where

import Test.QuickCheck

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)

genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
