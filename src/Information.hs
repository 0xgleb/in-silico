module Information
  (
  )
  where

import GoldenStandard

mutualInformation :: (Eq a, Num a) => [[a]] -> Int -> Int -> a
mutualInformation library i j
  = foldl _ 0 library

  where
    uniquePairs = removeDuplicates
      [ (x, y)
      | x <- library
      , y <- library
      ]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = removeDupTail []
  where
    removeDupTail accum = \case
      (x:xs) ->
        removeDupTail (x : accum) $ filter (/= x) xs

      [] -> accum
