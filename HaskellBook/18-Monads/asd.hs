module Trial where

data Apple = Core Name Weight deriving (Eq, Show)

type Name = String
type Weight = Int

mkName :: String -> Maybe String
mkName name' =
  case name' of
    "apple" -> Just "apple"
    _ -> Nothing

mkWeight :: Int -> Maybe Int
mkWeight mass =
  if (mass < 5)
  then Just mass
  else Nothing

