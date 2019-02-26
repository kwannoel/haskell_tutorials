module Monoidsss where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x@_ = x
  mappend x@_ Nada = x
  mappend (Only x) (Only y) = Only (mappend x y)
