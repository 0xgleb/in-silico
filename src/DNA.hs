module DNA
  ( DNANucs
  -- , DNANuc
  , DNANucleotide
  , pattern A
  , pattern C
  , pattern T
  , pattern G

  , parseDNANucs

  , complement
  , reverseComplement
  )
  where

import qualified Nucleotide as Nuc

import qualified Prelude
import           Protolude       hiding (complement)
import           Test.QuickCheck

type DNANucs
  = '[Nuc.A, Nuc.C, Nuc.T, Nuc.G]

-- class (nuc `Elem` DNANucs ~ 'True) => DNANuc (nuc :: Nucleotide) where

-- instance DNANuc 'A where
-- instance DNANuc 'C where
-- instance DNANuc 'T where
-- instance DNANuc 'G where

newtype DNANucleotide
  = DNANucleotide { getDNANucleotide :: Nuc.Nucleotide }
  deriving newtype (Show, Eq)
  deriving newtype (Arbitrary)

pattern A :: DNANucleotide
pattern A = DNANucleotide Nuc.A

pattern C :: DNANucleotide
pattern C = DNANucleotide Nuc.C

pattern T :: DNANucleotide
pattern T = DNANucleotide Nuc.T

pattern G :: DNANucleotide
pattern G = DNANucleotide Nuc.G

{-# COMPLETE A, C, T, G #-}

mkDNANucleotide :: Nuc.Nucleotide -> Maybe DNANucleotide
mkDNANucleotide = \case
  Nuc.A -> Just A
  Nuc.C -> Just C
  Nuc.T -> Just T
  Nuc.G -> Just G
  _     -> Nothing


parseDNANucs :: Prelude.String -> Maybe [DNANucleotide]
parseDNANucs
  = sequence . fmap mkDNANucleotide <=< Nuc.parseNucs


complement :: [DNANucleotide] -> [DNANucleotide]
complement = fmap $ \case
  A -> T
  C -> G
  T -> A
  G -> C

reverseComplement :: [DNANucleotide] -> [DNANucleotide]
reverseComplement = reverse . complement

-- complement :: NucList DNANuc -> NucList DNANuc
-- complement = nucMap $ \case
--   TSin -> ASin
--   ASin -> TSin
--   CSin -> GSin
--   GSin -> CSin

-- reverseComplement :: NucList DNANuc -> NucList DNANuc
-- reverseComplement = reverseNucList . complement
