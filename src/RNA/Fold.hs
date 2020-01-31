module RNA.Fold
  ( Score(..)

  , Fold(..)
  , showFold
  , flatPair

  , foldAndShow
  , finalScore
  , score
  , scoreWatsonCrick
  )
  where

import GoldenStandard
import RNA.Molecule

import qualified Data.Map.Lazy as Map
import qualified Prelude
import qualified Debug.Trace as Debug

data Fold
  = Pair [Fold]
  | UnpairedNuc
  deriving (Show, Eq)

showFold :: [Fold] -> Text
showFold = foldMap showAFold

  where
    showAFold = \case
      Pair innerFold ->
        "(" <> showFold innerFold <> ")"

      UnpairedNuc ->
        "."


newtype Score
  = Score { getScore :: Integer }
  deriving newtype (Show, Eq, Ord, Num)

flatPair :: Fold
flatPair = Pair []

foldAndShow :: [Nucleotide] -> Text
foldAndShow nucs
  = let folds = snd $ finalScore nucs
     in foldMap show nucs <> "\n" <> showFold folds

finalScore :: [Nucleotide] -> (Score, [Fold])
finalScore nucs
  = fromMaybe (Prelude.error "No score for the full sequence!")
  $ Map.lookup nucs
  $ scoreWatsonCrick nucs

scoreWatsonCrick :: [Nucleotide] -> Map [Nucleotide] (Score, [Fold])
scoreWatsonCrick = score $ \x y ->
  if x == complement y then 1 else 0

-- | Approximate energy level of the native fold using Nussinov Algorithm
score
  :: (Nucleotide -> Nucleotide -> Score)
  -> [Nucleotide]
  -> Map [Nucleotide] (Score, [Fold])

score scorePair rnaSeq
  = combine 2 rnaSeq initialScoreMap

  where
    combine
      :: Int
      -> [Nucleotide]
      -> Map [Nucleotide] (Score, [Fold])
      -> Map [Nucleotide] (Score, [Fold])

    combine n nucs scoreMap
      | n > length rnaSeq = scoreMap

      | length cur /= n = combine (n + 1) rnaSeq scoreMap

      | otherwise
      = let
          folder pp@(_, prev) nn@(_, next)
            = if fst prev <= fst next then nn else pp

          (msg, res)
            = foldl folder ("", (0, []))
            [ ("paired", pairedScore)
            , ("unpaired left",  unpairedScore True  $ drop 1 cur)
            , ("unpaired right", unpairedScore False $ take (n - 1) cur)
            , ("bifurcation", bifurcation (0, []) 1)
            ]

        in combine n (drop 1 nucs)
             $ flip (Map.insert cur) scoreMap
             $ debugTrace (msg <> ": " <> show @_ @Text cur) res

      where
        cur = take n nucs

        debugTrace :: Show a => Text -> a -> a
        debugTrace comment val
          = if False -- show @_ @Text cur == "[A,U,G,G,C]"
            then Debug.traceShow (comment <> ": " <> show @_ @Text val) val
            else val

        pairedScore :: (Score, [Fold])
        pairedScore
          = Map.lookup (drop 1 $ take (n - 1) cur) scoreMap

          & let pair = fromMaybe 0 $ scorePair <$> headMay cur <*> lastMay cur

            in  maybe (pair, [Pair []])
                  $ \(innerScore, innerFold) -> (innerScore + pair, [Pair innerFold])

        unpairedScore :: Bool -> [Nucleotide] -> (Score, [Fold])
        unpairedScore isInTheBeginning key
          = let (innerScore, innerFold) = fromMaybe (0, []) $ Map.lookup key scoreMap
            in  if isInTheBeginning
                   then (innerScore, UnpairedNuc : innerFold)
                   else (innerScore, innerFold <> [UnpairedNuc])

        bifurcation :: (Score, [Fold]) -> Int -> (Score, [Fold])
        bifurcation maxScore size
          | size == n = maxScore

          | currentScore >= fst maxScore
          = bifurcation current $ size + 1

          | otherwise
          = bifurcation maxScore $ size + 1

          where
            (firstScore, firstFold)
              = fromMaybe (0, []) (Map.lookup (take size cur) scoreMap)

            (secondScore, secondFold)
              = fromMaybe (0, []) (Map.lookup (drop size cur) scoreMap)

            current@(currentScore, _)
              = ( firstScore + secondScore
                , debugTrace "newFold" $ debugTrace "firstFold" firstFold <> debugTrace "secondFold" secondFold
                )

    initialScoreMap
      = Map.insert [A] (0, [UnpairedNuc])
      . Map.insert [U] (0, [UnpairedNuc])
      . Map.insert [G] (0, [UnpairedNuc])
      . Map.insert [C] (0, [UnpairedNuc])
      $ mempty
