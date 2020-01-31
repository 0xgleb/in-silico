module DNA
  ( Nucleotide
  , pattern A
  , pattern C
  , pattern T
  , pattern G

  , mkDNANucleotide
  , parseDNASeq
  , mkDoubleStrandedDNA

  , reverseComplement
  , getCodingStrand
  )
  where

import           Complementary
import           GoldenStandard
import qualified Nucleotide     as Nuc

import qualified Prelude

newtype Nucleotide
  = Nucleotide Nuc.Nucleotide
  deriving newtype (Show, Eq)

pattern A :: Nucleotide
pattern A = Nucleotide Nuc.A

pattern C :: Nucleotide
pattern C = Nucleotide Nuc.C

pattern T :: Nucleotide
pattern T = Nucleotide Nuc.T

pattern G :: Nucleotide
pattern G = Nucleotide Nuc.G

{-# COMPLETE A, C, T, G #-}

mkDNANucleotide :: Nuc.Nucleotide -> Either Text Nucleotide
mkDNANucleotide = \case
  Nuc.A -> Right A
  Nuc.C -> Right C
  Nuc.T -> Right T
  Nuc.G -> Right G

  nuc ->
    Left $ "Invalid DNA nucleotide: " <> show nuc


parseDNASeq :: Prelude.String -> Either Text [Nucleotide]
parseDNASeq
  = sequence . fmap mkDNANucleotide <=< Nuc.parseNucs

mkDoubleStrandedDNA :: [Nucleotide] -> [(Nucleotide, Nucleotide)]
mkDoubleStrandedDNA strand
  = zip strand $ reverseComplement strand


instance HasComplement Nucleotide where
  complement = \case
    A -> T
    C -> G
    T -> A
    G -> C

reverseComplement :: [Nucleotide] -> [Nucleotide]
reverseComplement = reverse . complement

getCodingStrand :: [DNA.Nucleotide] -> Either Text [DNA.Nucleotide]
getCodingStrand strand
  = finder strand

  where
    finder current = case current of
      A : T : G : _ ->
        Right strand

      T : A : C : _ ->
        Right $ complement strand

      _ : ns ->
        finder ns

      [] ->
        Left "Can't find a start codon or its complement!"
