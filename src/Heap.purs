module Heap
  ( class Heap
  , empty
  , isEmpty
  , insert
  , insertList
  , merge
  , findMin
  , deleteMin
  ) where


import Prelude (class Ord)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

-- needed for insertList
import Data.List (List(..))


class Heap h where
  empty     :: forall a. Ord a => h a
  isEmpty   :: forall a. Ord a => h a -> Boolean

  insert    :: forall a. Ord a => a -> h a -> h a
  merge     :: forall a. Ord a => h a -> h a -> h a

  findMin   :: forall a. Ord a => h a -> Maybe a
  deleteMin :: forall a. Ord a => h a -> Maybe (Tuple a (h a))


insertList :: forall a h. (Ord a, Heap h) => List a -> h a -> h a
insertList Nil heap = heap
insertList (Cons a as) heap = insertList as (insert a heap)
