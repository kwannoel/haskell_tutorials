{-
1. Nope
2. Yes
3. Yes
-}

newtype Mu f =
  InF { outF :: f (Mu f) }
-- This is a recursive type lmao wtf which does not terminate
-- InF (f (InF (f (InF (...)))))
-- 4) in terms of kinds yes, Mu has kind * -> *
-- no functor since f is of kind (* -> *)

data D =
  D ([] Word) Int Int deriving (Eq, Show)

-- D is kind * the nested values cannot change.

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data K a b =
  K a

instance Functor(K a) where
  fmap f (K x) = K x

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa (fx) (gx)) = DaWrappa (fmap f (fx)) (fmap f (gx))

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething (fa) (gb)) = IgnoringSomething fa $ fmap f gb

data List a =
    Nil
  | Cons a (List a)

instance (Functor List) where
  fmap _ Nil = Nil
  fmap f (Cons a lista) = Cons (f a) (fmap f lista)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance (Functor GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (MoreGoats g1 g2 g3) = (MoreGoats (fmap f g1)
                                           (fmap f g2)
                                           (fmap f g3))

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read func) = Read (f . func)


