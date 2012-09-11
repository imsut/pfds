module TrinomialHeap where

import Heap

data Tree a = Node a [(Tree a, Tree a)] deriving Show
data Digit a = Zero | One (Tree a) | Two (Tree a, Tree a) deriving Show
newtype TrinomialHeap a = TH [Digit a] deriving Show

rank :: Tree a -> Int
rank (Node _ ts) = length ts

root :: Ord a => Digit a -> a
root Zero = undefined
root (One (Node v _)) = v
root (Two (Node v1 _, Node v2 _))
  | v1 < v2   = v1
  | otherwise = v2

link :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
link t1@(Node v1 c1) t2@(Node v2 c2) t3@(Node v3 c3)
  | v1 <= v2 && v1 <= v3 = Node v1 ((t2, t3):c1)
  | v2 <= v3 && v2 <= v1 = Node v2 ((t3, t1):c2)
  | v3 <= v1 && v3 <= v2 = Node v3 ((t1, t2):c3)

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t []         = [One t]
insTree t (Zero:ts)  = One t : ts
insTree t ts@(One t' : ts')
  | rank t < rank t' = One t : ts
  | otherwise        = Two (t, t') : ts'
insTree t ts@(Two (t1, t2) : ts')
  | rank t < rank t1 = One t : ts
  | otherwise        = insTree (link t t1 t2) ts'

mrg :: Ord a => [Digit a] -> [Digit a] -> [Digit a]
mrg ds1 [] = ds1
mrg [] ds2 = ds2
mrg ds1@(Zero : ds1') ds2@(Zero : ds2') = mrg ds1' ds2'
mrg ds1@(d1 : ds1') ds2@(Zero : ds2') = d1 : mrg ds1' ds2
mrg ds1@(Zero : ds1') ds2@(d2 : ds2') = d2 : mrg ds1 ds2'
mrg ds1@(One t1 : ds1') ds2@(One t2 : ds2')
  | rank t1 < rank t2 = One t1 : mrg ds1' ds2
  | rank t1 > rank t2 = One t2 : mrg ds1 ds2'
  | otherwise         = Two (t1, t2) : mrg ds1' ds2'
mrg ds1@(One t1 : ds1') ds2@(Two (t2', t2'') : ds2')
  | rank t1 < rank t2' = One t1 : mrg ds1' ds2
  | rank t1 > rank t2' = Two (t2', t2'') : mrg ds1 ds2'
  | otherwise          = One (link t1 t2' t2'') : mrg ds1' ds2'
mrg ds1@(Two (t1', t1'') : ds1') ds2@(One t2 : ds2')
  | rank t1' < rank t2 = Two (t1', t1'') : mrg ds1' ds2
  | rank t1' > rank t2 = One t2 : mrg ds1 ds2'
  | otherwise          = One (link t1' t1'' t2) : mrg ds1' ds2'
mrg ds1@(Two (t1', t1'') : ds1') ds2@(Two (t2', t2'') : ds2')
  | rank t1' < rank t2' = Two (t1', t1'') : ds2
  | rank t1' > rank t2' = Two (t2', t2'') : ds1
  | otherwise           = One t1' : One (link t1'' t2' t2'') : mrg ds1' ds2'

removeMinDigit :: Ord a => [Digit a] -> (Digit a, [Digit a])
removeMinDigit [] = error "empty heap"
removeMinDigit (Zero : ds) = removeMinDigit ds
removeMinDigit (d : []) = (d, [])
removeMinDigit (d : ds)
  | root d < root d' = (d, ds)
  | otherwise        = (d', d : ds')
  where
    (d', ds') = removeMinDigit ds

deleteMin' :: Ord a => (Digit a, [Digit a]) -> [Digit a]
deleteMin' (Zero, _) = error "removeMinDigit shouldn't return (Zero, _)"
deleteMin' (One (Node _ ts), ds') = mrg ds' $ map Two $ reverse ts
deleteMin' (Two (t1@(Node v1 ts1), t2@(Node v2 ts2)), ds')
  | v1 < v2   = mrg (insTree t2 ds') (map Two $ reverse ts1)
  | otherwise = mrg (insTree t1 ds') (map Two $ reverse ts2)

instance Heap TrinomialHeap where
  empty = TH []
  isEmpty (TH ds) = null ds

  insert x (TH ds) = TH (insTree (Node x []) ds)
  merge (TH ds1) (TH ds2) = TH (mrg ds1 ds2)

  findMin (TH ds) = case removeMinDigit ds of
      (Zero, _) -> error "removeMinDigit shouldn't return (Zero, _)"
      (d, _)    -> root d
  deleteMin (TH ds) = TH (deleteMin' $ removeMinDigit ds)
