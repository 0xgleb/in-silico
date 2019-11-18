{-# LANGUAGE AllowAmbiguousTypes   #-}

-- the crazy stuff... it doesn't work

module Sin
  ( NucleotideSin(..)

  , Elem

  , NucList(..)
  , nucFunc

  , reverseNucList
  )
  where

import GoldenStandard
import Nucleotide

data NucleotideSin (a :: Nucleotide) where
  ASin :: NucleotideSin A
  CSin :: NucleotideSin C
  TSin :: NucleotideSin T
  GSin :: NucleotideSin G
  USin :: NucleotideSin U

data NucList (subset :: [Nucleotide]) where
  NucNil
    :: NucList subset

  NucCons
    :: nuc `Elem` subset
    => NucleotideSin nuc
    -> NucList subset
    -> NucList subset


type family ElemBool (x :: k) (xs :: [k]) :: Bool where
  x `ElemBool` '[]      = False
  x `ElemBool` (x : xs) = True
  x `ElemBool` (y : xs) = x `ElemBool` xs

type Elem (x :: k) (xs :: [k])
  = ElemBool x xs ~ True

type family ForallInList (list :: [k]) (c :: k -> Constraint) :: Constraint where
  ForallInList '[]      c = ()
  ForallInList (x : xs) c = (c x, ForallInList xs c)

instance Semigroup (NucList c) where
  NucNil <> list = list

  (n `NucCons` ns) <> list
    = n `NucCons` (ns <> list)

class
  ( prev `Elem` PrevSet tag
  , Next tag prev `Elem` NextSet tag
  )
  => NucMap (tag :: k) (prev :: Nucleotide) where

  type family PrevSet tag :: [Nucleotide]
  type family NextSet tag :: [Nucleotide]

  type family Next tag prev :: Nucleotide

  nucFuncProxy
    :: Proxy tag
    -> NucleotideSin prev
    -> NucleotideSin (Next tag prev)

nucFunc
  :: forall tag prev. NucMap tag prev
  => NucleotideSin prev
  -> NucleotideSin (Next tag prev)

nucFunc
  = nucFuncProxy $ Proxy @tag


-- nucMap
--   :: forall tag. (ForallInList (PrevSet tag) (NucMap tag))

--   => NucList (PrevSet tag)
--   -> NucList (NextSet tag)

-- nucMap = \case
--   NucNil ->
--     NucNil

--   NucCons n ns ->
--     nucFunc @tag n
--       `NucCons` nucMap @tag ns

singletonNuc :: nuc `Elem` subset => NucleotideSin nuc -> NucList subset
singletonNuc = flip NucCons NucNil

reverseNucList :: NucList c -> NucList c
reverseNucList = reverseNucTail NucNil
  where
    reverseNucTail :: NucList c -> NucList c -> NucList c
    reverseNucTail accum = \case
      NucNil ->
        accum

      NucCons n ns ->
        reverseNucTail (accum <> singletonNuc n) ns
