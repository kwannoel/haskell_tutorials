module Laws where

a :: Maybe String
a = const <$> Just "Hello" <*> pure "World"

b :: Maybe (Int, Int, String, [Int])
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

--laws

-- identity law : pure id <*> v == v
-- pure id <*> [1..5] = [] id <*> [1..5] = [id 1 .. id 5]
-- pure id <*> Nothing = Just id <*> Nothing = Nothing
-- pure id <*> (+1) $ 2 = id . (+1) $ 2

-- composition law : pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
-- . :: (b -> c) -> (a -> b) -> (a -> c)

-- homomorphism law : pure f <*> pure x = pure (f x)
-- pure (+1) <*> pure 1
-- pure ((+1) 1)
-- (+1) 1

-- Interchange law : u <*> pure y = pure ($ y) <*> u
-- [(+1), (*2)] <*> pure 1 = [1, 2] = pure ($ 1) <*> [(+1), (*2)]


