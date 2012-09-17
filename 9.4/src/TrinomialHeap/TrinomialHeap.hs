module TrinomialHeap
       (
       -- export data constructor for testing purpose
         TrinomialHeap(..)
       , module Heap

       -- export only for debugging purpose
       , mrg
       , removeMinTree
       , Tree(..)
       , Digit(..)
       ) where

import Heap

data Tree a = Node a [(Tree a, Tree a)] deriving Show
data Digit a = Zero | One (Tree a) | Two (Tree a, Tree a) deriving Show
newtype TrinomialHeap a = TH [Digit a] deriving Show

link :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
link t1@(Node v1 c1) t2@(Node v2 c2) t3@(Node v3 c3)
  | v1 <= v2 && v1 <= v3 = Node v1 ((t2, t3):c1)
  | v2 <= v3 && v2 <= v1 = Node v2 ((t3, t1):c2)
  | v3 <= v1 && v3 <= v2 = Node v3 ((t1, t2):c3)
  | otherwise            = error "shouldn't fail here"

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t []                   = One t : []
insTree t (Zero : ts)          = One t : ts
insTree t (One t' : ts')       = Two (t, t') : ts'
insTree t (Two (t1, t2) : ts') = Zero : insTree (link t t1 t2) ts'

add :: Ord a => Digit a -> Digit a -> (Digit a, Digit a)
add x Zero = (x, Zero)
add Zero x = (x, Zero)
add (One t1) (One t2) = (Two (t1, t2), Zero)
add (One t1) (Two (t2a, t2b)) = (Zero, One $ link t1 t2a t2b)
add (Two (t1a, t1b)) (One t2) = (Zero, One $ link t1a t1b t2)
-- any one of t1a, t1b, t2a, and t2b can appear as fst of a tuple to be returned
add (Two (t1a, t1b)) (Two (t2a, t2b)) = (One t1a, One $ link t1b t2a t2b)

mrg :: Ord a => [Digit a] -> [Digit a] -> [Digit a]
mrg ds1 [] = ds1
mrg [] ds2 = ds2
mrg (d1 : ds1) (d2 : ds2) = sumd : rest
  where
    (sumd, carryd) = add d1 d2
    rest = case carryd of
      Zero  -> mrg ds1 ds2
      One t -> insTree t (mrg ds1 ds2)
      _     -> error "carry cannot be Two"

removeMinTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
removeMinTree [] = error "empty list of digits"
removeMinTree [Zero] = error "Zero shouldn't appear as the most significant digit"
removeMinTree [One t] = (t, [])
removeMinTree [Two (t1@(Node v1 _), t2@(Node v2 _))]
  | v1 < v2   = (t1, [One t2])
  | otherwise = (t2, [One t1])
removeMinTree (Zero : ds) = (t, Zero : ds')
  where
    (t, ds') = removeMinTree ds
removeMinTree (d@(One t@(Node v _)) : ds)
  | v < v'    = (t, Zero : ds)
  | otherwise = (t', d : ds')
  where
    (t'@(Node v' _), ds') = removeMinTree ds
removeMinTree (d@(Two (t1@(Node v1 _), t2@(Node v2 _))) : ds)
  | v1 <= v2 && v1 < v' = (t1, One t2 : ds)
  | v2 <  v1 && v2 < v' = (t2, One t1 : ds)
  | otherwise           = (t', d : ds')
  where
    (t'@(Node v' _), ds') = removeMinTree ds

instance Heap TrinomialHeap where
  empty = TH []
  isEmpty (TH ds) = null ds

  insert x (TH ds) = TH (insTree (Node x []) ds)
  merge (TH ds1) (TH ds2) = TH (mrg ds1 ds2)

  findMin (TH ds) = case removeMinTree ds of
    (Node v _, _) -> v
  deleteMin (TH ds) = case removeMinTree ds of
    (Node _ c, ds') -> TH $ mrg (map Two $ reverse c) ds'
