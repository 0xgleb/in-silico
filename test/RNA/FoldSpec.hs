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

          expectedMap :: Map [Nucleotide] (Score, [Fold])
          expectedMap
            = Map.insert [A] (0, [UnpairedNuc])
            . Map.insert [U] (0, [UnpairedNuc])
            . Map.insert [G] (0, [UnpairedNuc])
            . Map.insert [C] (0, [UnpairedNuc])

            . Map.insert [A, U] (1, [flatPair])
            . Map.insert [U, G] (0, [UnpairedNuc, UnpairedNuc])
            . Map.insert [G, G] (0, [UnpairedNuc, UnpairedNuc])
            . Map.insert [G, C] (1, [flatPair])
            . Map.insert [C, A] (0, [UnpairedNuc, UnpairedNuc])
            . Map.insert [A, U] (1, [flatPair])
            . Map.insert [U, C] (0, [UnpairedNuc, UnpairedNuc])
            . Map.insert [C, G] (1, [flatPair])
            . Map.insert [G, C] (1, [flatPair])

            . Map.insert [A, U, G] (1, [flatPair, UnpairedNuc])
            . Map.insert [U, G, G] (0, [UnpairedNuc, UnpairedNuc, UnpairedNuc])
            . Map.insert [G, G, C] (1, [UnpairedNuc, flatPair])
            . Map.insert [G, C, A] (1, [flatPair, UnpairedNuc])
            . Map.insert [C, A, U] (1, [UnpairedNuc, flatPair])
            . Map.insert [A, U, C] (1, [flatPair, UnpairedNuc])
            . Map.insert [U, C, G] (1, [UnpairedNuc, flatPair])
            . Map.insert [C, G, G] (1, [flatPair, UnpairedNuc])
            . Map.insert [G, G, C] (1, [UnpairedNuc, flatPair])

            . Map.insert [A, U, G, G] (1, [flatPair, UnpairedNuc, UnpairedNuc])
            . Map.insert [U, G, G, C] (1, [UnpairedNuc, UnpairedNuc, flatPair])
            . Map.insert [G, G, C, A] (1, [UnpairedNuc, flatPair, UnpairedNuc])
            . Map.insert [G, C, A, U] (2, [flatPair, flatPair])
            . Map.insert [C, A, U, C] (1, [UnpairedNuc, flatPair, UnpairedNuc])
            . Map.insert [A, U, C, G] (2, [flatPair, flatPair])
            . Map.insert [U, C, G, G] (1, [UnpairedNuc, flatPair, UnpairedNuc])
            . Map.insert [C, G, G, C] (2, [flatPair, flatPair])

            . Map.insert [A, U, G, G, C] (2, [flatPair, UnpairedNuc, flatPair])
            . Map.insert [U, G, G, C, A] (2, [Pair [UnpairedNuc, flatPair]])
            . Map.insert [G, G, C, A, U] (2, [UnpairedNuc, flatPair, flatPair])
            . Map.insert [G, C, A, U, C] (2, [flatPair, flatPair, UnpairedNuc])
            . Map.insert [C, A, U, C, G] (2, [UnpairedNuc, flatPair, flatPair])
            . Map.insert [A, U, C, G, G] (2, [flatPair, flatPair, UnpairedNuc])
            . Map.insert [U, C, G, G, C] (2, [UnpairedNuc, flatPair, flatPair])


            . Map.insert [A, U, G, G, C, A]
                (2, [flatPair, UnpairedNuc, flatPair, UnpairedNuc])

            . Map.insert [U, G, G, C, A, U]
                (2, [Pair [UnpairedNuc, flatPair], UnpairedNuc])

            . Map.insert [G, G, C, A, U, C]
                (3, [Pair [flatPair, flatPair]])

            . Map.insert [G, C, A, U, C, G]
                (3, [flatPair, flatPair, flatPair])

            . Map.insert [C, A, U, C, G, G]
                (3, [Pair [flatPair, flatPair]])

            . Map.insert [A, U, C, G, G, C]
                (3, [flatPair, flatPair, flatPair])


            . Map.insert [A, U, G, G, C, A, U]
                (3, [flatPair, UnpairedNuc, flatPair, flatPair])

            . Map.insert [U, G, G, C, A, U, C]
                (3, [UnpairedNuc, Pair [flatPair, flatPair]])

            . Map.insert [G, G, C, A, U, C, G]
                (3, [Pair [flatPair, flatPair], UnpairedNuc])

            . Map.insert [G, C, A, U, C, G, G]
                (3, [flatPair, flatPair, flatPair, UnpairedNuc])

            . Map.insert [C, A, U, C, G, G, C]
                (3, [Pair [flatPair, flatPair], UnpairedNuc])


            . Map.insert [A, U, G, G, C, A, U, C]
                (4, [flatPair, Pair [flatPair, flatPair]])

            . Map.insert [U, G, G, C, A, U, C, G]
                (3, [UnpairedNuc, Pair [flatPair, flatPair], UnpairedNuc])

            . Map.insert [G, G, C, A, U, C, G, G]
                (3, [Pair [flatPair, flatPair], UnpairedNuc, UnpairedNuc])

            . Map.insert [G, C, A, U, C, G, G, C]
                (4, [flatPair, flatPair, flatPair, flatPair])


            . Map.insert [A, U, G, G, C, A, U, C, G]
                (4, [Pair [], Pair [Pair [], Pair []], UnpairedNuc])

            . Map.insert [U, G, G, C, A, U, C, G, G]
                (3, [UnpairedNuc, Pair [flatPair, flatPair], UnpairedNuc, UnpairedNuc])

            . Map.insert [G, G, C, A, U, C, G, G, C]
                (4, [Pair [flatPair, flatPair], UnpairedNuc, flatPair])


            . Map.insert [A, U, G, G, C, A, U, C, G, G]
                (4, [flatPair, Pair [flatPair, flatPair], UnpairedNuc, UnpairedNuc])

            . Map.insert [U, G, G, C, A, U, C, G, G, C]
                (4, [UnpairedNuc, Pair [flatPair, flatPair], UnpairedNuc, flatPair])

            . Map.insert [A, U, G, G, C, A, U, C, G, G, C]
                (5, [flatPair, Pair [flatPair, flatPair], UnpairedNuc, flatPair])

            $ mempty

          diff s e = if s == e then Nothing else Just s

      Map.differenceWith diff (scoreWatsonCrick rna) expectedMap
        `shouldBe` mempty
