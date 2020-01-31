module RNA.MoleculeSpec (spec) where

import           Complementary
import           GoldenStandard
import qualified Nucleotide     as Nuc
import qualified RNA

import qualified Data.Map.Lazy as Map
import qualified Prelude

import NucleotideSpec  ()
import Test.Hspec
import Test.QuickCheck


instance Arbitrary RNA.Nucleotide where
  arbitrary = do
    nuc <- arbitrary

    case RNA.mkRNANucleotide nuc of
      Right rnaNuc ->
        pure rnaNuc

      Left _ ->
        arbitrary


sampleGene :: Prelude.String
sampleGene
  = "UCAGACUGGUGCCGUGGUGCUCUCGCCCGAUGUGACGUCGACCGCCAGCGGCGCGAUGACGCCGAGGAUUUCCGU\
    \GAUCGUUUCGGAGGGCACGCCGGCUGCGGUCAGCGCGUCGGCCAAGUGUCCGGCGACCAGGCUGAAGUGGUGCAU\
    \GGUAAUUCCGCGCCCCUGAUGGACUUGCUUCAUCGGCGCACCGGUAUAGGGCUCGGGCCCGCCAAGCGCGGCCGC\
    \GAAAAACUCCACCUGCUUGCCCUUGAGGCGGCUCAUGUUCGUACCGCUGAAGAAGGCCGAUAGUUGGUCAUCGGC\
    \AAGCACACGAACAUAGAAGUCCUCGACGACGACUUCGAUGGCCUCAUGCCCGCCGAUCUUGUCGUAGAUGCUGAU\
    \CGGCUCACGUUUGCGCAAGCGUGACAGUAGUCCCAUUUUUAUA"

sampleComplement :: Prelude.String
sampleComplement
  = "AGUCUGACCACGGCACCACGAGAGCGGGCUACACUGCAGCUGGCGGUCGCCGCGCUACUGCGGCUCCUAAAGGCA\
    \CUAGCAAAGCCUCCCGUGCGGCCGACGCCAGUCGCGCAGCCGGUUCACAGGCCGCUGGUCCGACUUCACCACGUA\
    \CCAUUAAGGCGCGGGGACUACCUGAACGAAGUAGCCGCGUGGCCAUAUCCCGAGCCCGGGCGGUUCGCGCCGGCG\
    \CUUUUUGAGGUGGACGAACGGGAACUCCGCCGAGUACAAGCAUGGCGACUUCUUCCGGCUAUCAACCAGUAGCCG\
    \UUCGUGUGCUUGUAUCUUCAGGAGCUGCUGCUGAAGCUACCGGAGUACGGGCGGCUAGAACAGCAUCUACGACUA\
    \GCCGAGUGCAAACGCGUUCGCACUGUCAUCAGGGUAAAAAUAU"

spec :: Spec
spec = do
  describe "complement" $ do
    it "calculates the RNA complement strand" $ do
      let Right rna = RNA.parseRNASeq sampleGene

          Right rnaComplement = RNA.parseRNASeq sampleComplement

      fmap complement rna `shouldBe` rnaComplement

    it "complement . complement === identity" $ property $ \(nuc :: RNA.Nucleotide) ->
      complement (complement nuc) == nuc
