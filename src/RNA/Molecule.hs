module RNA.Molecule
  ( Nucleotide
  , pattern A
  , pattern C
  , pattern U
  , pattern G

  , fromDNA
  , mkRNANucleotide
  , parseRNASeq
  , complement
  )
  where

import qualified DNA
import           GoldenStandard
import qualified Nucleotide     as Nuc

import qualified Prelude

newtype Nucleotide
  = Nucleotide Nuc.Nucleotide
  deriving newtype (Show, Eq, Ord)

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
  | CodingStrandErr Text
  deriving (Show, Eq)

fromDNA
  :: [DNA.Nucleotide]
  -> Either TranscriptionError [Nucleotide]

fromDNA dna
  = first CodingStrandErr (DNA.getCodingStrand dna) >>= findTATA <&> fmap \case
      DNA.A -> A
      DNA.C -> C
      DNA.T -> U
      DNA.G -> G

  where
    findTATA = \case
      nucs@(DNA.T : DNA.A : DNA.T : DNA.A : DNA.A : DNA.A : _) ->
        Right nucs

      _ : ns ->
        findTATA ns

      [] ->
        Left NoTATABox


mkRNANucleotide :: Nuc.Nucleotide -> Either Text Nucleotide
mkRNANucleotide = \case
  Nuc.A -> Right A
  Nuc.C -> Right C
  Nuc.U -> Right U
  Nuc.G -> Right G

  nuc ->
    Left $ "Invalid RNA nucleotide: " <> show nuc


parseRNASeq :: Prelude.String -> Either Text [Nucleotide]
parseRNASeq
  = sequence . fmap mkRNANucleotide <=< Nuc.parseNucs

complement :: Nucleotide -> Nucleotide
complement = \case
  A -> U
  U -> A
  C -> G
  G -> C
