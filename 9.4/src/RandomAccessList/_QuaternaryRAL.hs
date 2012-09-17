module QuaternaryRAL
       (
         QuaternaryRAL(..)
       , module RandomAccessList

       -- export for test
       , Tree(..)
       , Digit(..)
       ) where

import RandomAccessList

{-
  No:  1  4 16 64
   1:  1
   5:  1  1
  20:  4  4
  39:  3  1  2
 100:  4  4  1  1

 Idx:  1  4 16 64
   0:  1
   1:  2
   3:  4
   4:  1  1
  19:  4  4
  20:  1  1  1
  39:  4  1  2
-}
data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) (Tree a) deriving Show
data Digit a = Digit1 (Tree a)
             | Digit2 (Tree a) (Tree a)
             | Digit3 (Tree a) (Tree a) (Tree a)
             | Digit4 (Tree a) (Tree a) (Tree a) (Tree a) deriving Show
newtype QuaternaryRAL a = QL [Digit a] deriving Show

consTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
consTree t [] = [Digit1 t]
consTree t (Digit1 t1 : ds) = Digit2 t t1 : ds
consTree t (Digit2 t1 t2 : ds) = Digit3 t t1 t2 : ds
consTree t (Digit3 t1 t2 t3 : ds) = Digit4 t t1 t2 t3 : ds
consTree t (Digit4 t1 t2 t3 t4 : ds) = Digit1 t : consTree (Node t1 t2 t3 t4) ds

unconsTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
unconsTree [] = error "empty list" -- better way to deal with an empty list?
unconsTree (Digit1 t : []) = (t, [])
unconsTree (Digit1 t : ds) = (t, Digit4 t1 t2 t3 t4 : ds')
  where
    (Node t1 t2 t3 t4, ds') = unconsTree ds
unconsTree (Digit2 t1 t2 : ds) = (t1, Digit1 t2 : ds)
unconsTree (Digit3 t1 t2 t3 : ds) = (t1, Digit2 t2 t3 : ds)
unconsTree (Digit4 t1 t2 t3 t4 : ds) = (t1, Digit3 t2 t3 t4 : ds)

lookupDigit :: Ord a => Int -> [Digit a] -> Int -> a
lookupDigit i [] _ = error "index out of bounds"
lookupDigit i (Digit1 t1 : ds) r
  | i < r * 1 = lookupTree (i - r * 0) t1 r
  | otherwise = lookupDigit (i - r * 1) ds (r * 4)
lookupDigit i (Digit2 t1 t2 : ds) r
  | i < r * 1 = lookupTree (i - r * 0) t1 r
  | i < r * 2 = lookupTree (i - r * 1) t2 r
  | otherwise = lookupDigit (i - r * 2) ds (r * 4)
lookupDigit i (Digit3 t1 t2 t3 : ds) r
  | i < r * 1 = lookupTree (i - r * 0) t1 r
  | i < r * 2 = lookupTree (i - r * 1) t2 r
  | i < r * 3 = lookupTree (i - r * 2) t3 r
  | otherwise = lookupDigit (i - r * 3) ds (r * 4)
lookupDigit i (Digit4 t1 t2 t3 t4 : ds) r
  | i < r * 1 = lookupTree (i - r * 0) t1 r
  | i < r * 2 = lookupTree (i - r * 1) t2 r
  | i < r * 3 = lookupTree (i - r * 2) t3 r
  | i < r * 4 = lookupTree (i - r * 3) t4 r
  | otherwise = lookupDigit (i - r * 4) ds (r * 4)

lookupTree :: Ord a => Int -> Tree a -> Int -> a
lookupTree 0 (Leaf v) _ = v
lookupTree i (Node t1 t2 t3 t4) n
  | i < m * 1 = lookupTree (i - m * 0) t1 m
  | i < m * 2 = lookupTree (i - m * 1) t2 m
  | i < m * 3 = lookupTree (i - m * 2) t3 m
  | otherwise = lookupTree (i - m * 3) t4 m
  where
    m = n `div` 4

updateDigit :: Ord a => Int -> a -> [Digit a] -> Int -> [Digit a]
updateDigit i _ [] _ = error "index out of bounds"
updateDigit i v (Digit1 t1 : ds) r
  | i < r * 1 = Digit1 (updateTree (i - r * 0) v t1 r) : ds
  | otherwise = Digit1 t1 : updateDigit (i - r * 1) v ds (r * 4)
updateDigit i v (Digit2 t1 t2 : ds) r
  | i < r * 1 = Digit2 (updateTree (i - r * 0) v t1 r) t2 : ds
  | i < r * 2 = Digit2 t1 (updateTree (i - r * 1) v t2 r) : ds
  | otherwise = Digit2 t1 t2 : updateDigit (i - r * 2) v ds (r * 4)
updateDigit i v (Digit3 t1 t2 t3 : ds) r
  | i < r * 1 = Digit3 (updateTree (i - r * 0) v t1 r) t2 t3 : ds
  | i < r * 2 = Digit3 t1 (updateTree (i - r * 1) v t2 r) t3 : ds
  | i < r * 3 = Digit3 t1 t2 (updateTree (i - r * 2) v t3 r) : ds
  | otherwise = Digit3 t1 t2 t3 : updateDigit (i - r * 3) v ds (r * 4)
updateDigit i v (Digit4 t1 t2 t3 t4 : ds) r
  | i < r * 1 = Digit4 (updateTree (i - r * 0) v t1 r) t2 t3 t4 : ds
  | i < r * 2 = Digit4 t1 (updateTree (i - r * 1) v t2 r) t3 t4 : ds
  | i < r * 3 = Digit4 t1 t2 (updateTree (i - r * 2) v t3 r) t4 : ds
  | i < r * 4 = Digit4 t1 t2 t3 (updateTree (i - r * 3) v t4 r) : ds
  | otherwise = Digit4 t1 t2 t3 t4 : updateDigit (i - r * 4) v ds (r * 4)

updateTree :: Ord a => Int -> a -> Tree a -> Int -> Tree a
updateTree 0 v (Leaf _) _ = (Leaf v)
updateTree i v (Node t1 t2 t3 t4) n
  | i < m * 1 = Node (updateTree (i - m * 0) v t1 m) t2 t3 t4
  | i < m * 2 = Node t1 (updateTree (i - m * 1) v t2 m) t3 t4
  | i < m * 3 = Node t1 t2 (updateTree (i - m * 2) v t3 m) t4
  | otherwise = Node t1 t2 t3 (updateTree (i - m * 3) v t4 m)
  where
    m = n `div` 4

instance RandomAccessList QuaternaryRAL where
  empty = QL []
  isEmpty (QL ls) = null ls

  cons x (QL ds) = QL $ consTree (Leaf x) ds

  head (QL []) = error "empty list"
  head (QL (Digit1 (Leaf v) : _)) = v
  head (QL (Digit2 (Leaf v) _ : _)) = v
  head (QL (Digit3 (Leaf v) _ _ : _)) = v
  head (QL (Digit4 (Leaf v) _ _ _ : _)) = v

  tail (QL ds) = QL $ (snd . unconsTree) ds

  lookup i (QL ds) = lookupDigit i ds 1
  update i v (QL ds) = QL $ updateDigit i v ds 1
