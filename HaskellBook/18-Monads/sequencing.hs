module Sequencing where

import Control.Monad
import Control.Applicative

seq' :: [] String -> [] String -> [] String
seq' a b = a >> b

seqIO :: IO ()
seqIO =
  putStrLn "123" *>
  putStrLn "!23"
