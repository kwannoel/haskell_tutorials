{-
class Foldable (t :: * -> *) where
  fold :: Monoid m => t m
  foldMap :: Monoid m
          => (a -> m) -> t a -> m
-}
