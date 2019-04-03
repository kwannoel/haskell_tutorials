module Begin where

import Control.Applicative

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

floop :: Num a => a -> a
floop = fmap boop doop

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop
  -- (+) :: a -> (a -> a)
  -- boop :: a -> a
  -- (+) <$> boop
  -- execute boop, we get a resultant value which we apply to (+) and get a parital func
  -- i.e. (a -> a)
  -- The overall function will be a -> (a -> a) on the LHS
  -- rewriting this as (->) a (a -> a)
  -- We can use ap
  -- ((->) a (a -> a)) -> ((->) a a)
  -- We combine the initial functions
  -- we should get a -> a
  -- First we apply incoming value 5 to both functions
  -- We get a partial function on the lhs
  -- We then apply this to rhs resultant value

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
