module RNA
  ( RNANucleotide
  , pattern A
  , pattern C
  , pattern U
  , pattern G

  , fromDNA
  )
  where

import qualified DNA
import qualified Nucleotide as Nuc

import Protolude       hiding (complement)
import Test.QuickCheck

newtype RNANucleotide
  = RNANucleotide Nuc.Nucleotide
  deriving newtype (Show, Eq)
  deriving newtype (Arbitrary)

pattern A :: RNANucleotide
pattern A = RNANucleotide Nuc.A

pattern C :: RNANucleotide
pattern C = RNANucleotide Nuc.C

pattern U :: RNANucleotide
pattern U = RNANucleotide Nuc.U

pattern G :: RNANucleotide
pattern G = RNANucleotide Nuc.G

{-# COMPLETE A, C, U, G #-}

data TranscriptionError
  = NoTATABox
  deriving (Show, Eq)

fromDNA
  :: [(DNA.DNANucleotide, DNA.DNANucleotide)]
  -> Either TranscriptionError [RNANucleotide]

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
