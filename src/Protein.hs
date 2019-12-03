module Protein
  ( Protein(..)
  , synthesizeProteins
  , getOpenReadingFrames
  )
  where

import           GoldenStandard
import qualified RNA
import Codon

newtype Protein
  = Protein { getAminoAcids :: [AminoAcid] }
  deriving newtype (Show, Eq)

synthesizeProteins :: [RNA.Nucleotide] -> Either CodonError [Protein]
synthesizeProteins
  = fmap (fmap Protein) . getOpenReadingFrames

getOpenReadingFrames :: [RNA.Nucleotide] -> Either CodonError [[AminoAcid]]
getOpenReadingFrames = getORF []

  where
    getORF
      :: [[AminoAcid]]
      -> [RNA.Nucleotide]
      -> Either CodonError [[AminoAcid]]

    getORF aaAccum nucs@(n1 : n2 : n3 : _)
      = case Codon.codonToAminoAcid (n1, n2, n3) of
          AminoAcid M -> do
            (orf, rest) <- readTillStop nucs
            getORF (aaAccum <> [orf]) rest

          _ ->
            getORF aaAccum $ drop 1 nucs

    getORF aaAccum _ = Right aaAccum

data CodonError
  = NoStopCodon
  deriving (Show, Eq)

readTillStop :: [RNA.Nucleotide] -> Either CodonError ([AminoAcid], [RNA.Nucleotide])
readTillStop = rts []

  where
    rts acids = \case
      n1 : n2 : n3 : ns ->
        case codonToAminoAcid (n1, n2, n3) of
          AminoAcid aa ->
            rts (acids <> [aa]) ns

          Stop ->
            Right (acids, ns)

      _ ->
        Left NoStopCodon
