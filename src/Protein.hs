module Protein
  ( synthesizeProteins
  , codonToProtein
  )
  where

import           GoldenStandard
import qualified RNA
import qualified Codon

data Protein
  = A -- ^ Alanine
  | C -- ^ Cysteine
  | D -- ^ Aspartic acid
  | E -- ^ Glutamic acid
  | F -- ^ Phenylalanine
  | G -- ^ Glycine
  | H -- ^ Histidine
  | I -- ^ Isoleucine
  | K -- ^ Lysine
  | L -- ^ Leucine
  | M -- ^ Methionine
  | N -- ^ Asparagine
  | P -- ^ Proline
  | Q -- ^ Glutamine
  | R -- ^ Arginine
  | S -- ^ Serine
  | T -- ^ Threonine
  | V -- ^ Valine
  | W -- ^ Tryptophan
  | Y -- ^ Tyrosine
  deriving (Show, Eq)

data ProteinBuilder
  = Protein Protein
  | Stop
  deriving (Show, Eq)

synthesizeProteins :: [RNA.Nucleotide] -> [Protein]
synthesizeProteins = synth []
  where
    synth proteinSeq (n1:n2:n3:ns)
      = case codonToProtein $ Codon.rnaTripletToCodon (n1, n2, n3) of
          Protein protein ->
            synth (proteinSeq <> [protein]) ns

          Stop ->
            proteinSeq

    synth proteinSeq _
      = proteinSeq


codonToProtein
  :: Codon.Codon
  -> ProteinBuilder

codonToProtein = \case
  Codon.AUG -> Protein M
  Codon.GCG -> Protein A
  Codon.UCA -> Protein S
  Codon.GAA -> Protein E
  Codon.GGG -> Protein G
  Codon.GGU -> Protein G
  Codon.AAA -> Protein K
  Codon.GAG -> Protein E
  Codon.AAU -> Protein N
  Codon.CUA -> Protein L
  Codon.CAU -> Protein H
  Codon.UCG -> Protein S
  Codon.UAG -> Stop
  Codon.GUG -> Protein V
  Codon.UAU -> Protein Y
  Codon.CCU -> Protein P
  Codon.ACU -> Protein T
  Codon.UCC -> Protein S
  Codon.CAG -> Protein Q
  Codon.CCA -> Protein P
  Codon.UAA -> Stop
  Codon.AGA -> Protein R
  Codon.ACG -> Protein T
  Codon.CAA -> Protein Q
  Codon.UGU -> Protein C
  Codon.GCU -> Protein A
  Codon.UUC -> Protein F
  Codon.AGU -> Protein S
  Codon.AUA -> Protein I
  Codon.UUA -> Protein L
  Codon.CCG -> Protein P
  Codon.AUC -> Protein I
  Codon.UUU -> Protein F
  Codon.CGU -> Protein R
  Codon.UGA -> Stop
  Codon.GUA -> Protein V
  Codon.UCU -> Protein S
  Codon.CAC -> Protein H
  Codon.GUU -> Protein V
  Codon.GAU -> Protein D
  Codon.CGA -> Protein R
  Codon.GGA -> Protein G
  Codon.GUC -> Protein V
  Codon.GGC -> Protein G
  Codon.UGC -> Protein C
  Codon.CUG -> Protein L
  Codon.CUC -> Protein L
  Codon.CGC -> Protein R
  Codon.CGG -> Protein R
  Codon.AAC -> Protein N
  Codon.GCC -> Protein A
  Codon.AUU -> Protein I
  Codon.AGG -> Protein R
  Codon.GAC -> Protein D
  Codon.ACC -> Protein T
  Codon.AGC -> Protein S
  Codon.UAC -> Protein Y
  Codon.ACA -> Protein T
  Codon.AAG -> Protein K
  Codon.GCA -> Protein A
  Codon.UUG -> Protein L
  Codon.CCC -> Protein P
  Codon.CUU -> Protein L
  Codon.UGG -> Protein W
