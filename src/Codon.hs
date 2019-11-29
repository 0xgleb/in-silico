module Codon
  ( Codon(..)
  , rnaTripletToCodon
  )
  where

import           GoldenStandard
import qualified RNA

data Codon
  = AUG
  | GCG
  | UCA
  | GAA
  | GGG
  | GGU
  | AAA
  | GAG
  | AAU
  | CUA
  | CAU
  | UCG
  | UAG
  | GUG
  | UAU
  | CCU
  | ACU
  | UCC
  | CAG
  | CCA
  | UAA
  | AGA
  | ACG
  | CAA
  | UGU
  | GCU
  | UUC
  | AGU
  | AUA
  | UUA
  | CCG
  | AUC
  | UUU
  | CGU
  | UGA
  | GUA
  | UCU
  | CAC
  | GUU
  | GAU
  | CGA
  | GGA
  | GUC
  | GGC
  | UGC
  | CUG
  | CUC
  | CGC
  | CGG
  | AAC
  | GCC
  | AUU
  | AGG
  | GAC
  | ACC
  | AGC
  | UAC
  | ACA
  | AAG
  | GCA
  | UUG
  | CCC
  | CUU
  | UGG
  deriving (Show)

rnaTripletToCodon
  :: (RNA.Nucleotide, RNA.Nucleotide, RNA.Nucleotide)
  -> Codon

rnaTripletToCodon = \case
  (RNA.A, RNA.U, RNA.G) -> AUG
  (RNA.G, RNA.C, RNA.G) -> GCG
  (RNA.U, RNA.C, RNA.A) -> UCA
  (RNA.G, RNA.A, RNA.A) -> GAA
  (RNA.G, RNA.G, RNA.G) -> GGG
  (RNA.G, RNA.G, RNA.U) -> GGU
  (RNA.A, RNA.A, RNA.A) -> AAA
  (RNA.G, RNA.A, RNA.G) -> GAG
  (RNA.A, RNA.A, RNA.U) -> AAU
  (RNA.C, RNA.U, RNA.A) -> CUA
  (RNA.C, RNA.A, RNA.U) -> CAU
  (RNA.U, RNA.C, RNA.G) -> UCG
  (RNA.U, RNA.A, RNA.G) -> UAG
  (RNA.G, RNA.U, RNA.G) -> GUG
  (RNA.U, RNA.A, RNA.U) -> UAU
  (RNA.C, RNA.C, RNA.U) -> CCU
  (RNA.A, RNA.C, RNA.U) -> ACU
  (RNA.U, RNA.C, RNA.C) -> UCC
  (RNA.C, RNA.A, RNA.G) -> CAG
  (RNA.C, RNA.C, RNA.A) -> CCA
  (RNA.U, RNA.A, RNA.A) -> UAA
  (RNA.A, RNA.G, RNA.A) -> AGA
  (RNA.A, RNA.C, RNA.G) -> ACG
  (RNA.C, RNA.A, RNA.A) -> CAA
  (RNA.U, RNA.G, RNA.U) -> UGU
  (RNA.G, RNA.C, RNA.U) -> GCU
  (RNA.U, RNA.U, RNA.C) -> UUC
  (RNA.A, RNA.G, RNA.U) -> AGU
  (RNA.A, RNA.U, RNA.A) -> AUA
  (RNA.U, RNA.U, RNA.A) -> UUA
  (RNA.C, RNA.C, RNA.G) -> CCG
  (RNA.A, RNA.U, RNA.C) -> AUC
  (RNA.U, RNA.U, RNA.U) -> UUU
  (RNA.C, RNA.G, RNA.U) -> CGU
  (RNA.U, RNA.G, RNA.A) -> UGA
  (RNA.G, RNA.U, RNA.A) -> GUA
  (RNA.U, RNA.C, RNA.U) -> UCU
  (RNA.C, RNA.A, RNA.C) -> CAC
  (RNA.G, RNA.U, RNA.U) -> GUU
  (RNA.G, RNA.A, RNA.U) -> GAU
  (RNA.C, RNA.G, RNA.A) -> CGA
  (RNA.G, RNA.G, RNA.A) -> GGA
  (RNA.G, RNA.U, RNA.C) -> GUC
  (RNA.G, RNA.G, RNA.C) -> GGC
  (RNA.U, RNA.G, RNA.C) -> UGC
  (RNA.C, RNA.U, RNA.G) -> CUG
  (RNA.C, RNA.U, RNA.C) -> CUC
  (RNA.C, RNA.G, RNA.C) -> CGC
  (RNA.C, RNA.G, RNA.G) -> CGG
  (RNA.A, RNA.A, RNA.C) -> AAC
  (RNA.G, RNA.C, RNA.C) -> GCC
  (RNA.A, RNA.U, RNA.U) -> AUU
  (RNA.A, RNA.G, RNA.G) -> AGG
  (RNA.G, RNA.A, RNA.C) -> GAC
  (RNA.A, RNA.C, RNA.C) -> ACC
  (RNA.A, RNA.G, RNA.C) -> AGC
  (RNA.U, RNA.A, RNA.C) -> UAC
  (RNA.A, RNA.C, RNA.A) -> ACA
  (RNA.A, RNA.A, RNA.G) -> AAG
  (RNA.G, RNA.C, RNA.A) -> GCA
  (RNA.U, RNA.U, RNA.G) -> UUG
  (RNA.C, RNA.C, RNA.C) -> CCC
  (RNA.C, RNA.U, RNA.U) -> CUU
  (RNA.U, RNA.G, RNA.G) -> UGG
