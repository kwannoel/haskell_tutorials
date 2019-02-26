module Semigroups where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

genTrivial :: Gen Trivial
genTrivial = return Trivial

instance Arbitrary Trivial where
  arbitrary = genTrivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

--
--

newtype Identity a = Identity a deriving (Eq, Show)

genId :: Arbitrary a => Gen (Identity a)
genId = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

type IdAssoc =
  Identity String -> Identity String -> Identity String -> Bool

--
--

data Two a b = Two a b deriving (Eq, Show)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two w x <> Two y z = Two (w <> y) (x <> z)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--
--

newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

genBoolConj :: Gen BoolConj
genBoolConj = do
  a <- arbitrary
  return (BoolConj a)

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True =
    BoolConj True
  _ <> _ = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--
--

data Or a b =
    Fst a
  | Snd b deriving (Eq, Show)

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a, return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

instance Semigroup (Or a b) where
  Fst _ <> a@(Fst _) = a
  Fst _ <> b@(Snd _) = b
  c@(Snd _) <> _     = c

type OrAssoc = Or String String
            -> Or String String
            -> Or String String
            -> Bool

--
--

newtype Combine a b =
  Combine { unCombine :: (a -> b)} 

instance Semigroup b => Semigroup (Combine a b) where
  Combine funcA <> Combine funcB = Combine (\n -> funcA n <> funcB n)

type FAtoB = (String -> String)
          -> Bool

genFunc :: Gen (String -> String)
genFunc = arbitrary

genStr :: Gen String
genStr = arbitrary

genFuncStr :: Gen (String -> String) -> Gen String -> Gen String
genFuncStr f x = do
  func <- f
  var  <- x
  return (func var)

genFuncStr' :: Gen (String -> Bool)
genFuncStr' = do
  let arbitraryF = (arbitrary :: Gen (String -> String))
      c = Combine
  f1 <- arbitraryF
  f2 <- arbitraryF
  f3 <- arbitraryF
  return (\n -> (unCombine ( (c f1 <> c f2) <> c f3 ) $ n ) ==
                (unCombine (c f1 <> (c f2 <> c f3) ) $ n )
         )
-- 1) function which takes  random function, applies assoc law
-- 2) function which then applies random vars to the above 
-- 3) After applying check LHS and RHS for equivalence
-- function which takes to above results and checks for assoc law

--
--

newtype Comp a =
  Comp { unComp :: (a -> a)}

genCom :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genCom = do
  a <- arbitrary
  return (Comp a)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genCom 

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

genComp :: Gen (String -> Bool)
genComp = do
  let arbitraryC = arbitrary :: Gen (Comp String)
  f1 <- arbitraryC
  f2 <- arbitraryC
  f3 <- arbitraryC
  return (\n -> ((unComp $ (f1 <> f2) <> f3 ) $ n )
             == ((unComp $ f3 <> (f1 <> f2)) $ n ))

--
--


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (genFuncStr') 
  quickCheck (genComp)
--function generates a set of function on opposite ends
--function


