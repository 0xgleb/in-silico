module Complementary
  ( HasComplement(..)
  )
  where

import GoldenStandard

class HasComplement a where
  complement :: a -> a

instance HasComplement a => HasComplement [a] where
  complement = fmap complement
