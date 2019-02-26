module HeavyLifting where

--1)

a = fmap (+1) $ read "[1]" :: [Int]

--2)

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3)

c = fmap (*2) (\x -> x - 2)

--4)

--fmap :: (a -> b) -> f a -> f b
--fmap :: ([Int] -> String) -> (((->) Int) [Int]) -> (((->) Int) String)
d :: Int -> String
d =
  fmap
  ((return '1' ++) . show)
  (\x -> [x, 1..3])

--fmap takes a function (a -> b) but cant change the structure, i.e. ((->) a) b
--it can however, change the inside of the function, i.e. the result, b
--hence, the function fmapped onto (a -> b) is applied to b. i.e. b -> c
--Thus this is a function composition. i.e. fmap f g == f . g,
--since g is applied to get b in (a -> b)
--and afterwards f is applied to get c i.e. (b -> c)
-- This produces the result (a -> c) which has kept the structure intact
-- i.e. ((->) a c)

--5)

-- 3 structures: IO Integer, String, function
-- IO Integer -> IO String
-- IO String -> IO String
-- IO String -> IO Integer
-- IO Integer -> IO Integer
-- concat 1 to 123, giving "1231"
-- read "1231" which gives 1231
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap ( read . ("123" ++) . show) ioi
    in fmap (*3) changed

-- functor two
data Two a b =
  Two a b deriving (Eq, Show)

--fmap :: (b -> c) -> (Two a b) -> (Two a c)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose f g v =
  (fmap (f . g) v) == (fmap f . fmap g $ v)
