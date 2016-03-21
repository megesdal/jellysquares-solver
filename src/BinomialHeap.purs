module BinomialHeap (BinomialHeap) where


import Prelude (class Ord, (+), (<), (<=), (<#>))
import Data.List (List(..), (:), null, reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)

import Heap


newtype BinomialHeap a = BH (List (BinomialTree a))


data BinomialTree a = Node Int a (List (BinomialTree a))


rank :: forall a. BinomialTree a -> Int
rank (Node r _ _) = r


root :: forall a. BinomialTree a -> a
root (Node _ x _) = x


link :: forall a. Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 <= x2 then
    Node (r + 1) x1 (t2 : c1)
  else
    Node (r + 1) x2 (t1 : c2)


insTree :: forall a. Ord a => BinomialTree a -> List (BinomialTree a) -> List (BinomialTree a)
insTree t Nil = t : Nil
insTree t ts@(Cons t' ts') =
  if rank t < rank t' then
    t : ts
  else -- they must be equal before they are stored in rank order...
    insTree (link t t') ts'


mrg :: forall a. Ord a => List (BinomialTree a) -> List (BinomialTree a) -> List (BinomialTree a)
mrg ts1 Nil = ts1
mrg Nil ts2 = ts2
mrg ts1@(Cons t1 ts1') ts2@(Cons t2 ts2') =
  if rank t1 < rank t2 then
    t1 : mrg ts1' ts2
  else if rank t2 < rank t1 then
    t2 : mrg ts1 ts2'
  else
    insTree (link t1 t2) (mrg ts1' ts2')


removeMinTree :: forall a. Ord a => List (BinomialTree a) -> Maybe (Tuple (BinomialTree a) (List (BinomialTree a)))
removeMinTree Nil = Nothing
removeMinTree (Cons t Nil) = Just (Tuple t Nil)
removeMinTree (Cons t ts) =
  let
    takeMin (Tuple t' ts') =
      if root t <= root t' then
        Tuple t ts
      else
        Tuple t' (t : ts')
  in
    removeMinTree ts <#> takeMin


instance binomialHeap :: Heap BinomialHeap where

  empty = BH Nil
  isEmpty (BH ts) = null ts

  insert x (BH ts) = BH (insTree (Node 0 x Nil) ts)
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

  -- TODO: make findMin O(1)
  findMin (BH ts) = removeMinTree ts <#> fst <#> root
  deleteMin (BH ts) =
    let
      addBackChildren (Tuple (Node _ x ts1) ts2) =
        Tuple x (BH (mrg (reverse ts1) ts2))
    in
      removeMinTree ts <#> addBackChildren
