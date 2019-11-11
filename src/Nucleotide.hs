{-# LANGUAGE AllowAmbiguousTypes #-}

module Nucleotide
  ( Nucleotide(..)
  , NucleotideSin(..)

  , NucList(..)
  -- , nucMap
  , reverseNucList

  , Elem

  , parseNucs
  , countNucs
  )
  where

import qualified Data.Map.Lazy   as Map
import qualified Prelude
import           Protolude
import qualified Safe
import           Test.QuickCheck

data Nucleotide
  = A
  | C
  | T
  | G
  | U
  deriving stock (Show, Read, Eq, Ord, Enum)

instance Arbitrary Nucleotide where
  arbitrary = toEnum . flip mod 4 . abs <$> arbitrary

data NucleotideSin (a :: Nucleotide) where
  ASin :: NucleotideSin A
  CSin :: NucleotideSin C
  TSin :: NucleotideSin T
  GSin :: NucleotideSin G
  USin :: NucleotideSin U

data NucList (c :: Nucleotide -> Constraint) where
  NucNil  :: NucList c
  NucCons :: c nuc => NucleotideSin nuc -> NucList c -> NucList c

instance Semigroup (NucList c) where
  NucNil <> list = list

  (n `NucCons` ns) <> list
    = n `NucCons` (ns <> list)

type family Function (tag :: k) (input :: arg) :: arg

-- nucMap
--   :: forall tag cPrev cNext
--    . ( forall nuc. (cPrev nuc, cNext (Function tag nuc))
--      => NucleotideSin nuc -> NucleotideSin (Function tag nuc)
--      )

--   -> NucList cPrev
--   -> NucList cNext

-- nucMap func = \case
--   NucNil ->
--     NucNil

--   NucCons n ns ->
--     n `NucCons` nucMap func ns

singletonNuc :: c nuc => NucleotideSin nuc -> NucList c
singletonNuc = flip NucCons NucNil

reverseNucList :: NucList c -> NucList c
reverseNucList = reverseNucTail NucNil
  where
    reverseNucTail :: NucList c -> NucList c -> NucList c
    reverseNucTail accum = \case
      NucNil ->
        accum

      NucCons n ns ->
        reverseNucTail (accum <> singletonNuc n) ns

type family Elem (x :: k) (xs :: [k]) :: Bool where
  x `Elem` '[]       = 'False
  x `Elem` (x ': xs) = 'True
  x `Elem` (y ': xs) = x `Elem` xs

parseNucs :: Prelude.String -> Maybe [Nucleotide]
parseNucs = sequence . fmap (Safe.readMay . pure)

countNucs :: [Nucleotide] -> Map Nucleotide Int
countNucs
  = flip foldr mempty $ Map.alter (Just . maybe 1 (+ 1))
