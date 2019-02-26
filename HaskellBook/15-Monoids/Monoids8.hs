module Monoids8 where

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }


-- Need to extract from f1, f2 the tuple
--Add them together through composition
--a) i.e. \x -> (a, f x) <> \y -> (a, g x) where a is the same since
--it does not have a semigroup constraint.
--however, f and g have the same types but maybe different expressions
--hence we combine them through composition i.e.
--cont. from a) RHS = \z -> (a, f . g $ z)
--hence the mempty for the above is \x -> (a, id x)
--to show a) in the semigroup instance of mem
--We have to extract f and g from their functions.

instance Semigroup a => Semigroup (Mem s a) where
  Mem f1 <> Mem f2 =
    Mem (\k ->
           let
             (a1, r1) = f1 k
             (a2, r2) = f2 r1
           in
             (a1 <> a2, r2) )
    
instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmRight = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmRight
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmRight == runMem f' 0
