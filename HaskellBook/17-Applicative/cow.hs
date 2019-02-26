module Cow where

import Control.Applicative

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
               }
           deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

mkCow :: String -> Int -> Int
      -> Maybe Cow

-- Cow :: String -> Int -> Int -> Cow

mkCow s i i' = Cow <$> noEmpty s <*> noNegative i <*> noNegative i'

mkCow' :: String -> Int -> Int
       -> Maybe Cow

mkCow' s i i' = liftA3 Cow (noEmpty s)
                           (noNegative i)
                           (noNegative i')
