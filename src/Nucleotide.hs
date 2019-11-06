module Nucleotide
  ( Nucleotide(..)
  , parseNucs
  , countNucs
  )
  where

import qualified Data.Map.Lazy as Map
import qualified Prelude
import           Protolude
import qualified Safe

data Nucleotide
  = A
  | C
  | T
  | G
  deriving (Show, Read, Eq, Ord)

data NucleotideSin (a :: Nucleotide) where
  ASin :: NucleotideSin A
  CSin :: NucleotideSin C
  TSin :: NucleotideSin T
  GSin :: NucleotideSin G

type family Elem (x :: Type) (xs :: [Type]) :: Bool where
  x `Elem` '[]       = 'False
  x `Elem` (x ': xs) = 'True
  x `Elem` (y ': xs) = x `Elem` xs

parseNucs :: Prelude.String -> Maybe [Nucleotide]
parseNucs = sequence . fmap (Safe.readMay . pure)

countNucs :: [Nucleotide] -> Map Nucleotide Int
countNucs
  = flip foldr mempty $ Map.alter (Just . maybe 1 (+ 1))
