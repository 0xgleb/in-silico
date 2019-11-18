module GoldenStandard
  ( module Protolude
  , trim
  , isEmpty
  )
  where

import Protolude hiding (complement, list)


trim :: (a -> Bool) -> [a] -> [a]
trim func
  = dropWhile func . takeWhile (not . func)


isEmpty :: Char -> Bool
isEmpty char
  =  char == ' '
  || char == '\n'
