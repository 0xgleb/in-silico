module GoldenStandard
  ( module Protolude
  , trim
  , isEmpty

  , Prelude.String
  , Prelude.lines

  , type ($)
  )
  where

import qualified Prelude
import           Protolude hiding (complement, list)


trim :: (a -> Bool) -> [a] -> [a]
trim func
  = dropWhile func . takeWhile (not . func)


isEmpty :: Char -> Bool
isEmpty char
  =  char == ' '
  || char == '\n'

type family ($) (func :: a -> b) (arg :: a) :: b where
  func $ arg = func arg
