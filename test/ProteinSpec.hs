module ProteinSpec (spec) where

import           GoldenStandard
import qualified Nucleotide     as Nuc
import qualified Protein
import qualified RNA

import qualified Data.Map.Lazy as Map
import qualified Prelude

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "synthesizeProteins" $ do
    it "returns the correct protein sequence" $ do
      rnaSeq <- Prelude.readFile "./test-assets/rna-test-seq.txt"
      proteinSeq <- Prelude.readFile "./test-assets/protein-expected-seq.txt"

      rna <- case RNA.parseRNASeq rnaSeq of
        Right rna -> pure rna
        Left  err -> panic err

      proteins <- case Protein.synthesizeProteins rna of
        Right ps  -> pure ps
        Left  err -> panic $ show err

      foldMap show (foldMap Protein.getAminoAcids proteins)
        `shouldBe` trim isEmpty proteinSeq
