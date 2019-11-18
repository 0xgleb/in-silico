module Protein
  ( synthesizeProteins
  , codoneToProtein
  )
  where

import           GoldenStandard
import qualified RNA

data Protein
  = A
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | K
  | L
  | M
  | N
  | P
  | Q
  | R
  | S
  | T
  | V
  | W
  | Y
  deriving (Show, Eq)

data ProteinBuilder
  = Protein Protein
  | Stop
  deriving (Show, Eq)

synthesizeProteins :: [RNA.Nucleotide] -> [Protein]
synthesizeProteins = synth []

  where
    synth proteinSeq (n1:n2:n3:ns)
      = case codoneToProtein (n1, n2, n3) of
          Protein protein ->
            synth (proteinSeq <> [protein]) ns

          Stop ->
            proteinSeq

    synth proteinSeq _
      = proteinSeq


codoneToProtein
  :: (RNA.Nucleotide, RNA.Nucleotide, RNA.Nucleotide)
  -> ProteinBuilder

codoneToProtein = \case
  (RNA.A, RNA.U, RNA.G) -> Protein M
  (RNA.G, RNA.C, RNA.G) -> Protein A
  (RNA.U, RNA.C, RNA.A) -> Protein S
  (RNA.G, RNA.A, RNA.A) -> Protein E
  (RNA.G, RNA.G, RNA.G) -> Protein G
  (RNA.G, RNA.G, RNA.U) -> Protein G
  (RNA.A, RNA.A, RNA.A) -> Protein K
  (RNA.G, RNA.A, RNA.G) -> Protein E
  (RNA.A, RNA.A, RNA.U) -> Protein N
  (RNA.C, RNA.U, RNA.A) -> Protein L
  (RNA.C, RNA.A, RNA.U) -> Protein H
  (RNA.U, RNA.C, RNA.G) -> Protein S
  (RNA.U, RNA.A, RNA.G) -> Stop
  (RNA.G, RNA.U, RNA.G) -> Protein V
  (RNA.U, RNA.A, RNA.U) -> Protein Y
  (RNA.C, RNA.C, RNA.U) -> Protein P
  (RNA.A, RNA.C, RNA.U) -> Protein T
  (RNA.U, RNA.C, RNA.C) -> Protein S
  (RNA.C, RNA.A, RNA.G) -> Protein Q
  (RNA.C, RNA.C, RNA.A) -> Protein P
  (RNA.U, RNA.A, RNA.A) -> Stop
  (RNA.A, RNA.G, RNA.A) -> Protein R
  (RNA.A, RNA.C, RNA.G) -> Protein T
  (RNA.C, RNA.A, RNA.A) -> Protein Q
  (RNA.U, RNA.G, RNA.U) -> Protein C
  (RNA.G, RNA.C, RNA.U) -> Protein A
  (RNA.U, RNA.U, RNA.C) -> Protein F
  (RNA.A, RNA.G, RNA.U) -> Protein S
  (RNA.A, RNA.U, RNA.A) -> Protein I
  (RNA.U, RNA.U, RNA.A) -> Protein L
  (RNA.C, RNA.C, RNA.G) -> Protein P
  (RNA.A, RNA.U, RNA.C) -> Protein I
  (RNA.U, RNA.U, RNA.U) -> Protein F
  (RNA.C, RNA.G, RNA.U) -> Protein R
  (RNA.U, RNA.G, RNA.A) -> Stop
  (RNA.G, RNA.U, RNA.A) -> Protein V
  (RNA.U, RNA.C, RNA.U) -> Protein S
  (RNA.C, RNA.A, RNA.C) -> Protein H
  (RNA.G, RNA.U, RNA.U) -> Protein V
  (RNA.G, RNA.A, RNA.U) -> Protein D
  (RNA.C, RNA.G, RNA.A) -> Protein R
  (RNA.G, RNA.G, RNA.A) -> Protein G
  (RNA.G, RNA.U, RNA.C) -> Protein V
  (RNA.G, RNA.G, RNA.C) -> Protein G
  (RNA.U, RNA.G, RNA.C) -> Protein C
  (RNA.C, RNA.U, RNA.G) -> Protein L
  (RNA.C, RNA.U, RNA.C) -> Protein L
  (RNA.C, RNA.G, RNA.C) -> Protein R
  (RNA.C, RNA.G, RNA.G) -> Protein R
  (RNA.A, RNA.A, RNA.C) -> Protein N
  (RNA.G, RNA.C, RNA.C) -> Protein A
  (RNA.A, RNA.U, RNA.U) -> Protein I
  (RNA.A, RNA.G, RNA.G) -> Protein R
  (RNA.G, RNA.A, RNA.C) -> Protein D
  (RNA.A, RNA.C, RNA.C) -> Protein T
  (RNA.A, RNA.G, RNA.C) -> Protein S
  (RNA.U, RNA.A, RNA.C) -> Protein Y
  (RNA.A, RNA.C, RNA.A) -> Protein T
  (RNA.A, RNA.A, RNA.G) -> Protein K
  (RNA.G, RNA.C, RNA.A) -> Protein A
  (RNA.U, RNA.U, RNA.G) -> Protein L
  (RNA.C, RNA.C, RNA.C) -> Protein P
  (RNA.C, RNA.U, RNA.U) -> Protein L
  (RNA.U, RNA.G, RNA.G) -> Protein W
