module QuickCheckTut where

import           Data.List       (sort)
import           Test.QuickCheck


half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

halfGen :: (Arbitrary a, Fractional a) => Gen a
halfGen = do
  a <- arbitrary
  return $ halfIdentity a


halfProp :: Property
halfProp =
  forAll (halfGen :: Gen Float)
  (\x -> half x == halfIdentity x  )

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

sortGen :: (Arbitrary a, Ord a) => Gen ([] a)
sortGen = do
  a <- arbitrary
  return $ sort a

listProp :: Property
listProp =
  forAll (sortGen :: Gen ([Int]))
  (\x -> listOrdered x)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

genTuple :: Gen (Int, Int)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: Gen (Int, Int, Int)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

passTwo :: (a -> b -> c) -> (a, b) -> c
passTwo f (x, y) = f x y

passThree :: (a -> b -> c -> d) -> (a, b, c) -> d
passThree f (x, y, z) = f x y z

assProp :: Property
assProp =
  forAll genThreeple (\x -> passThree plusAssociative x)

powerAss :: Int -> Int -> Int -> Bool
powerAss a b c =
  a ^ (b ^ c) == (a ^ b) ^ c

powerCom :: Int -> Int -> Bool
powerCom a b =
  a ^ b == b ^ a

propPowerAss :: Property
propPowerAss =
  forAll genThreeple (\x -> passThree powerAss x)


propPowerCom :: Property
propPowerCom =
  forAll genTuple (\x -> passTwo powerCom x)

genList :: Gen ([Int])
genList = arbitrary

reverseX2Prop :: Property
reverseX2Prop = forAll genList (\x -> (reverse . reverse $ x) == x)

dollarProp :: Property
dollarProp = forAll (arbitrary :: Gen Int) (\x -> (id $ x ) == id x)

genList' :: Gen ([Int], [Int])
genList' = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

foldrProp :: Property
foldrProp = forAll genList' (\x -> passTwo (foldr (:)) x == passTwo (++) x )

main :: IO ()
main = do
  quickCheck propPowerAss
  quickCheck propPowerCom
  quickCheck assProp
  quickCheck reverseX2Prop
  quickCheck dollarProp
  quickCheck foldrProp

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = genFool

genFool :: Gen Fool
genFool = oneof [return Fulse, return Frue]

genFool23 :: Gen Fool
genFool23 = frequency [(2, return Fulse), (3, return Frue)]
