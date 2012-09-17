module RandomAccessList
       (
         RandomAccessList(..)
       ) where

class RandomAccessList l where
  empty         :: Ord a => l a
  isEmpty       :: Ord a => l a -> Bool

  cons          :: Ord a => a -> l a -> l a
  head          :: Ord a => l a -> a
  tail          :: Ord a => l a -> l a

  lookup        :: Ord a => Int -> l a -> a
  update        :: Ord a => Int -> a -> l a -> l a
