module Nucleotide
  ( Nucleotide(..)

  , parseNucs
  , countNucs
  )
  where

import GoldenStandard

import qualified Data.Map.Lazy   as Map
import qualified Data.Text       as Tx
import qualified Prelude
import qualified Safe
import           Test.QuickCheck

data Nucleotide
  = A | C | T | G | U
  deriving stock (Show, Read, Eq, Ord, Enum)

instance Arbitrary Nucleotide where
  arbitrary = toEnum . flip mod 4 . abs <$> arbitrary

parseNucs :: Prelude.String -> Either Text [Nucleotide]
parseNucs
  = sequence
  . fmap (either (Left . Tx.pack) Right . Safe.readEitherSafe . pure)
  . trim isEmpty

countNucs :: [Nucleotide] -> Map Nucleotide Int
countNucs
  = flip foldr mempty $ Map.alter (Just . maybe 1 (+ 1))
