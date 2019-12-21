module RNA.FoldSpec (spec) where

import GoldenStandard
import RNA

import qualified Data.Map.Internal.Debug as Map.Debug
import qualified Data.Map.Lazy           as Map

import Test.Hspec

rnaSeq :: String
rnaSeq = "AUGGCAUCGGC"

spec :: Spec
spec = do
  describe "score" $ do
    it "returns the number of hydrogen bonds in the optimal RNA fold" $ do
      let Right rna = parseRNASeq rnaSeq

          expectedMap :: Map [Nucleotide] Score
          expectedMap
            = Map.insert [A, U] 1
            . Map.insert [U, G] 0
            . Map.insert [G, G] 0
            . Map.insert [G, C] 1
            . Map.insert [C, A] 0
            . Map.insert [A, U] 1
            . Map.insert [U, C] 0
            . Map.insert [C, G] 1
            . Map.insert [G, C] 1

            . Map.insert [A, U, G] 1
            . Map.insert [U, G, G] 0
            . Map.insert [G, G, C] 1
            . Map.insert [G, C, A] 1
            . Map.insert [C, A, U] 1
            . Map.insert [A, U, C] 1
            . Map.insert [U, C, G] 1
            . Map.insert [C, G, G] 1
            . Map.insert [G, G, C] 1

            . Map.insert [A, U, G, G] 1
            . Map.insert [U, G, G, C] 1
            . Map.insert [G, G, C, A] 1
            . Map.insert [G, C, A, U] 2
            . Map.insert [C, A, U, C] 1
            . Map.insert [A, U, C, G] 2
            . Map.insert [U, C, G, G] 1
            . Map.insert [C, G, G, C] 2

            . Map.insert [A, U, G, G, C] 2
            . Map.insert [U, G, G, C, A] 1
            . Map.insert [G, G, C, A, U] 2
            . Map.insert [G, C, A, U, C] 2
            . Map.insert [C, A, U, C, G] 2
            . Map.insert [A, U, C, G, G] 2
            . Map.insert [U, C, G, G, C] 2

            . Map.insert [A, U, G, G, C, A] 2
            . Map.insert [U, G, G, C, A, U] 2
            . Map.insert [G, G, C, A, U, C] 3
            . Map.insert [G, C, A, U, C, G] 3
            . Map.insert [C, A, U, C, G, G] 3
            . Map.insert [A, U, C, G, G, C] 3

            . Map.insert [A, U, G, G, C, A, U] 3
            . Map.insert [U, G, G, C, A, U, C] 3
            . Map.insert [G, G, C, A, U, C, G] 3
            . Map.insert [G, C, A, U, C, G, G] 3
            . Map.insert [C, A, U, C, G, G, C] 3

            . Map.insert [A, U, G, G, C, A, U, C] 4
            . Map.insert [U, G, G, C, A, U, C, G] 3
            . Map.insert [G, G, C, A, U, C, G, G] 3
            . Map.insert [G, C, A, U, C, G, G, C] 4

            . Map.insert [A, U, G, G, C, A, U, C, G] 4
            . Map.insert [U, G, G, C, A, U, C, G, G] 3
            . Map.insert [G, G, C, A, U, C, G, G, C] 4

            . Map.insert [A, U, G, G, C, A, U, C, G, G] 4
            . Map.insert [U, G, G, C, A, U, C, G, G, C] 4

            . Map.insert [A, U, G, G, C, A, U, C, G, G, C] 5

            $ mempty

          diff s e = if s == e then Nothing else Just s

      Map.differenceWith diff (score rna) expectedMap `shouldBe` mempty
