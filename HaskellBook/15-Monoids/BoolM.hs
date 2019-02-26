module BoolM where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools

instance Semigroup Bull where
  (<>) _ _ = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc x y z =
  x <> ( y <> z ) == ( x <> y ) <> z

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity x = x <> mempty == x

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity x = mempty <> x == x

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
  a <- arbitrary
  frequency [(1, return Nada), (1, return $ Only a)]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional
  
instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only x) (Only y) = Only ((<>) x y)


newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (<>) a (First' Nada) = a
  (<>) (First' Nada) a = a
  (<>) t@(First' (Only _ )) _ = t
  (<>) _ t@(First' (Only _ )) = t 

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return (First' a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
