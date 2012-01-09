module LeftistHeap (module Heap, LeftistHeap, insertL, deleteMinL) where

import Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving Show

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: Ord a => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b
  | rank a >= rank b 	= T (rank b + 1) x a b
  | otherwise		= T (rank a + 1) x b a

insertL :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insertL x E = T 1 x E E
insertL x h@(T _ y l r)
  | x <= y    = makeT x h E
  | otherwise = makeT y r $ insertL x l

mergeL :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
mergeL h E = merge h E
mergeL E h = merge E h
mergeL h1@(T _ x l1 r1) h2@(T _ y l2 r2)
    | x <= y	= makeT x r1 (mergeL l1 h2)
    | otherwise = makeT y r2 (mergeL h1 l2)

deleteMinL :: Ord a => LeftistHeap a -> LeftistHeap a
deleteMinL E           = error "empty heap"
deleteMinL (T _ _ a b) = mergeL a b

instance Heap LeftistHeap where
  empty 	= E
  isEmpty E 	= True
  isEmpty _	= False

--  insert x h	= merge (T 1 x E E) h

  insert x E	= T 1 x E E
  insert x h@(T _ y l r)
    | x <= y	= makeT x h E
    | otherwise = makeT y l $ insert x r

  merge h E	= h
  merge E h	= h
  merge h1@(T _ x l1 r1) h2@(T _ y l2 r2)
    | x <= y	= makeT x l1 (merge r1 h2)
    | otherwise = makeT y l2 (merge h1 r2)

  findMin E	= error "empty heap"
  findMin (T _ x _ _) = x

  deleteMin E 	= error "empty heap"
  deleteMin (T _ _ a b) = merge a b
