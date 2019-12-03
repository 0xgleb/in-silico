module Codon
  ( DecodedCodon(..)
  , AminoAcid(..)
  , codonToAminoAcid
  )
  where

import           GoldenStandard
import qualified RNA

data AminoAcid
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

data DecodedCodon
  = AminoAcid AminoAcid
  | Stop
  deriving (Show, Eq)

codonToAminoAcid
  :: (RNA.Nucleotide, RNA.Nucleotide, RNA.Nucleotide)
  -> DecodedCodon

codonToAminoAcid = \case
  (RNA.A, RNA.U, RNA.G) -> AminoAcid M
  (RNA.G, RNA.C, RNA.G) -> AminoAcid A
  (RNA.U, RNA.C, RNA.A) -> AminoAcid S
  (RNA.G, RNA.A, RNA.A) -> AminoAcid E
  (RNA.G, RNA.G, RNA.G) -> AminoAcid G
  (RNA.G, RNA.G, RNA.U) -> AminoAcid G
  (RNA.A, RNA.A, RNA.A) -> AminoAcid K
  (RNA.G, RNA.A, RNA.G) -> AminoAcid E
  (RNA.A, RNA.A, RNA.U) -> AminoAcid N
  (RNA.C, RNA.U, RNA.A) -> AminoAcid L
  (RNA.C, RNA.A, RNA.U) -> AminoAcid H
  (RNA.U, RNA.C, RNA.G) -> AminoAcid S
  (RNA.U, RNA.A, RNA.G) -> Stop
  (RNA.G, RNA.U, RNA.G) -> AminoAcid V
  (RNA.U, RNA.A, RNA.U) -> AminoAcid Y
  (RNA.C, RNA.C, RNA.U) -> AminoAcid P
  (RNA.A, RNA.C, RNA.U) -> AminoAcid T
  (RNA.U, RNA.C, RNA.C) -> AminoAcid S
  (RNA.C, RNA.A, RNA.G) -> AminoAcid Q
  (RNA.C, RNA.C, RNA.A) -> AminoAcid P
  (RNA.U, RNA.A, RNA.A) -> Stop
  (RNA.A, RNA.G, RNA.A) -> AminoAcid R
  (RNA.A, RNA.C, RNA.G) -> AminoAcid T
  (RNA.C, RNA.A, RNA.A) -> AminoAcid Q
  (RNA.U, RNA.G, RNA.U) -> AminoAcid C
  (RNA.G, RNA.C, RNA.U) -> AminoAcid A
  (RNA.U, RNA.U, RNA.C) -> AminoAcid F
  (RNA.A, RNA.G, RNA.U) -> AminoAcid S
  (RNA.A, RNA.U, RNA.A) -> AminoAcid I
  (RNA.U, RNA.U, RNA.A) -> AminoAcid L
  (RNA.C, RNA.C, RNA.G) -> AminoAcid P
  (RNA.A, RNA.U, RNA.C) -> AminoAcid I
  (RNA.U, RNA.U, RNA.U) -> AminoAcid F
  (RNA.C, RNA.G, RNA.U) -> AminoAcid R
  (RNA.U, RNA.G, RNA.A) -> Stop
  (RNA.G, RNA.U, RNA.A) -> AminoAcid V
  (RNA.U, RNA.C, RNA.U) -> AminoAcid S
  (RNA.C, RNA.A, RNA.C) -> AminoAcid H
  (RNA.G, RNA.U, RNA.U) -> AminoAcid V
  (RNA.G, RNA.A, RNA.U) -> AminoAcid D
  (RNA.C, RNA.G, RNA.A) -> AminoAcid R
  (RNA.G, RNA.G, RNA.A) -> AminoAcid G
  (RNA.G, RNA.U, RNA.C) -> AminoAcid V
  (RNA.G, RNA.G, RNA.C) -> AminoAcid G
  (RNA.U, RNA.G, RNA.C) -> AminoAcid C
  (RNA.C, RNA.U, RNA.G) -> AminoAcid L
  (RNA.C, RNA.U, RNA.C) -> AminoAcid L
  (RNA.C, RNA.G, RNA.C) -> AminoAcid R
  (RNA.C, RNA.G, RNA.G) -> AminoAcid R
  (RNA.A, RNA.A, RNA.C) -> AminoAcid N
  (RNA.G, RNA.C, RNA.C) -> AminoAcid A
  (RNA.A, RNA.U, RNA.U) -> AminoAcid I
  (RNA.A, RNA.G, RNA.G) -> AminoAcid R
  (RNA.G, RNA.A, RNA.C) -> AminoAcid D
  (RNA.A, RNA.C, RNA.C) -> AminoAcid T
  (RNA.A, RNA.G, RNA.C) -> AminoAcid S
  (RNA.U, RNA.A, RNA.C) -> AminoAcid Y
  (RNA.A, RNA.C, RNA.A) -> AminoAcid T
  (RNA.A, RNA.A, RNA.G) -> AminoAcid K
  (RNA.G, RNA.C, RNA.A) -> AminoAcid A
  (RNA.U, RNA.U, RNA.G) -> AminoAcid L
  (RNA.C, RNA.C, RNA.C) -> AminoAcid P
  (RNA.C, RNA.U, RNA.U) -> AminoAcid L
  (RNA.U, RNA.G, RNA.G) -> AminoAcid W
