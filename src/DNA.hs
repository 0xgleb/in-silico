module DNA
  ( Nucleotide
  , pattern A
  , pattern C
  , pattern T
  , pattern G

  , parseDNANucs
  , mkDoubleStrandedDNA

  , complement
  , reverseComplement
  )
  where

import           GoldenStandard
import qualified Nucleotide     as Nuc

import qualified Prelude
import           Test.QuickCheck

newtype Nucleotide
  = Nucleotide Nuc.Nucleotide
  deriving newtype (Show, Eq)
  deriving newtype (Arbitrary)

pattern A :: Nucleotide
pattern A = Nucleotide Nuc.A

pattern C :: Nucleotide
pattern C = Nucleotide Nuc.C

pattern T :: Nucleotide
pattern T = Nucleotide Nuc.T

pattern G :: Nucleotide
pattern G = Nucleotide Nuc.G

{-# COMPLETE A, C, T, G #-}

mkDNANucleotide :: Nuc.Nucleotide -> Maybe Nucleotide
mkDNANucleotide = \case
  Nuc.A -> Just A
  Nuc.C -> Just C
  Nuc.T -> Just T
  Nuc.G -> Just G
  _     -> Nothing


parseDNANucs :: Prelude.String -> Maybe [Nucleotide]
parseDNANucs
  = sequence . fmap mkDNANucleotide <=< Nuc.parseNucs

mkDoubleStrandedDNA :: [Nucleotide] -> [(Nucleotide, Nucleotide)]
mkDoubleStrandedDNA strand
  = zip strand $ reverseComplement strand


complement :: [Nucleotide] -> [Nucleotide]
complement = fmap $ \case
  A -> T
  C -> G
  T -> A
  G -> C

reverseComplement :: [Nucleotide] -> [Nucleotide]
reverseComplement = reverse . complement
