module Heap
  ( class Heap
  , empty
  , isEmpty
  , insert
  , merge
  , findMin
  , deleteMin
  ) where


import Prelude (class Ord)
import Data.Maybe


class Heap h where
  empty     :: forall a. Ord a => h a
  isEmpty   :: forall a. Ord a => h a -> Boolean

  insert    :: forall a. Ord a => a -> h a -> h a
  merge     :: forall a. Ord a => h a -> h a -> h a

  findMin   :: forall a. Ord a => h a -> Maybe a
  deleteMin :: forall a. Ord a => h a -> Maybe (h a)
