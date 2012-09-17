module SkewTrinaryRAL
       (
         SkewTrinaryRAL(..)
       , module RandomAccessList

       -- export for test
       ) where

import RandomAccessList

data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a) deriving Show
data SkewTrinaryRAL a = STL [(Int, Tree a)] deriving Show

lookupList :: Int -> [(Int, Tree a)] -> a
lookupList _ [] = error "index out of bounds"
lookupList i ((w, t) : ts)
  | i < w     = lookupTree w i t
  | otherwise = lookupList (i - w) ts

lookupTree :: Int -> Int -> Tree a -> a
lookupTree 1 0 (Leaf x) = x
lookupTree 1 _ (Leaf _) = error "index out of bounds"
lookupTree _ _ (Leaf _) = error "Leaf must have 1 for its weight"
lookupTree _ 0 (Node x _ _ _) = x
lookupTree w i (Node _ t1 t2 t3)
  | i <= w' * 1 = lookupTree w' (i - 1 - w' * 0) t1
  | i <= w' * 2 = lookupTree w' (i - 1 - w' * 1) t2
  | otherwise   = lookupTree w' (i - 1 - w' * 2) t3
  where
    w' = w `div` 3

updateList :: Int -> a -> [(Int, Tree a)] -> [(Int, Tree a)]
updateList _ _ [] = error "index out of bounds"
updateList i v ((w, t) : ts)
  | i < w     = (w, updateTree w i v t) : ts
  | otherwise = (w, t) : updateList (i - w) v ts

updateTree :: Int -> Int -> a -> Tree a -> Tree a
updateTree 1 0 v (Leaf _) = Leaf v
updateTree 1 _ _ (Leaf _) = error "index out of bounds"
updateTree _ _ _ (Leaf _) = error "Leaf must have 1 for its weight"
updateTree _ 0 v (Node _ t1 t2 t3) = Node v t1 t2 t3
updateTree w i v (Node x t1 t2 t3)
  | i <= w' * 1 = Node x (updateTree w' (i - 1 - w' * 0) v t1) t2 t3
  | i <= w' * 2 = Node x t1 (updateTree w' (i - 1 - w' * 1) v t2) t3
  | otherwise   = Node x t1 t2 (updateTree w' (i - 1 - w' * 2) v t3)
  where
    w' = w `div` 3

instance RandomAccessList SkewTrinaryRAL where
  empty = STL []
  isEmpty (STL ts) = null ts

  cons v (STL ts@((w1, t1) : (w2, t2) : (w3, t3) : ts'))
    | w1 == w2 && w2 == w3 = STL $ (1 + w1 + w2 + w3, Node v t1 t2 t3) : ts'
    | otherwise            = STL $ (1, Leaf v) : ts
  cons v (STL ts) = STL $ (1, Leaf v) : ts

  head (STL []) = error "empty list"
  head (STL ((1, Leaf x) : _)) = x
  head (STL ((_, Leaf _) : _)) = error "Leaf must have 1 for its weight"
  head (STL ((_, Node x _ _ _) : _)) = x

  tail (STL []) = error "empty list"
  tail (STL ((1, Leaf _) : ts)) = STL ts
  tail (STL ((_, Leaf _) : _)) = error "Leaf must have 1 for its weight"
  tail (STL ((w, Node _ t1 t2 t3) : ts)) = STL $ (v, t1) : (v, t2) : (v, t3) : ts
    where
      v = w `div` 3

  lookup i (STL ts) = lookupList i ts
  update i v (STL ts) = STL $ updateList i v ts
