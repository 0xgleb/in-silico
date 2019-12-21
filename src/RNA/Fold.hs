module RNA.Fold
  ( Score(..)
  , score
  )
  where

import GoldenStandard
import RNA.Molecule

import qualified Data.Map.Lazy as Map

newtype Score
  = Score { getScore :: Integer }
  deriving newtype (Show, Eq, Ord, Num)

-- | Approximate energy level of the native fold using Nussinov Algorithm
score :: [Nucleotide] -> Map [Nucleotide] Score
score rnaSeq
  = combine 3 rnaSeq $ scorePairs rnaSeq mempty

  where
    combine
      :: Int
      -> [Nucleotide]
      -> Map [Nucleotide] Score
      -> Map [Nucleotide] Score

    combine n nucs scoreMap
      | n > length rnaSeq = scoreMap

      | length cur /= n = combine (n + 1) rnaSeq scoreMap

      | otherwise
      = flip (Map.insert cur) scoreMap
      $ foldl max 0
          [ pairedScore
          , unpairedScore $ drop 1 cur
          , unpairedScore $ take (n - 1) cur
          , bifurcation 0 1
          ]

      where
        cur = take n nucs

        pairedScore
          = maybe 1 (+ 1) $ Map.lookup (drop 1 $ take (n - 1) cur) scoreMap

        unpairedScore key
          = fromMaybe 0 $ Map.lookup key scoreMap

        bifurcation :: Score -> Int -> Score
        bifurcation maxScore size
          | size == n = maxScore

          | currentScore > maxScore
          = bifurcation currentScore $ size + 1

          | otherwise
          = bifurcation maxScore $ size + 1

          where
            firstPart  = take size cur
            secondPart = drop size cur

            currentScore
              = fromMaybe 0 (Map.lookup firstPart  scoreMap)
              + fromMaybe 0 (Map.lookup secondPart scoreMap)


    scorePairs
      :: [Nucleotide]
      -> Map [Nucleotide] Score
      -> Map [Nucleotide] Score

    scorePairs (x : y : rest) scoreMap
      = scorePairs (y : rest) (newMap scoreMap)

      where
        newMap = Map.alter upd [x, y]

        upd = Just . \case
          Nothing  -> Score $ if x == complement y then 1 else 0
          Just val -> val

    scorePairs _ scoreMap = scoreMap
