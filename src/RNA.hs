module RNA
  ( Nucleotide
  , pattern A
  , pattern C
  , pattern U
  , pattern G

  , fromDNA
  )
  where

import qualified DNA
import           GoldenStandard
import qualified Nucleotide     as Nuc

import Test.QuickCheck

newtype Nucleotide
  = Nucleotide Nuc.Nucleotide
  deriving newtype (Show, Eq)
  deriving newtype (Arbitrary)

pattern A :: Nucleotide
pattern A = Nucleotide Nuc.A

pattern C :: Nucleotide
pattern C = Nucleotide Nuc.C

pattern U :: Nucleotide
pattern U = Nucleotide Nuc.U

pattern G :: Nucleotide
pattern G = Nucleotide Nuc.G

{-# COMPLETE A, C, U, G #-}

data TranscriptionError
  = NoTATABox
  deriving (Show, Eq)

fromDNA
  :: [(DNA.Nucleotide, DNA.Nucleotide)]
  -> Either TranscriptionError [Nucleotide]

fromDNA dna
  = codingStrand <&> fmap \case
      DNA.A -> A
      DNA.C -> C
      DNA.T -> U
      DNA.G -> G

  where
    (firstStrand, secondStrand) = unzip dna

    tataBox
      = [DNA.T, DNA.A, DNA.T, DNA.A]

    codingStrand
      | take 4 firstStrand == tataBox
      = Right firstStrand

      | take 4 secondStrand == tataBox
      = Right secondStrand

      | otherwise
      = Left NoTATABox
