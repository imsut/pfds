module QuaternaryRAL
       (
         QuaternaryRAL(..)
       , module RandomAccessList

       -- export for test
       , Tree(..)
       , Digit(..)
       ) where

import Data.Monoid (mappend)

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

data Context a = Context { frontDigits :: [Digit a]
                         , rearDigits :: [Digit a]
                         , digitContext :: DigitWalk a
                         , treeContext :: [TreeWalk a]
                         }
data DigitWalk a = Digit1Go1
                 | Digit2Go1 (Tree a)
                 | Digit2Go2 (Tree a)
                 | Digit3Go1 (Tree a) (Tree a)
                 | Digit3Go2 (Tree a) (Tree a)
                 | Digit3Go3 (Tree a) (Tree a)
                 | Digit4Go1 (Tree a) (Tree a) (Tree a)
                 | Digit4Go2 (Tree a) (Tree a) (Tree a)
                 | Digit4Go3 (Tree a) (Tree a) (Tree a)
                 | Digit4Go4 (Tree a) (Tree a) (Tree a)
data TreeWalk a = GoDown1 (Tree a) (Tree a) (Tree a)
                | GoDown2 (Tree a) (Tree a) (Tree a)
                | GoDown3 (Tree a) (Tree a) (Tree a)
                | GoDown4 (Tree a) (Tree a) (Tree a)

initialCtx :: [Digit a] -> Context a
initialCtx ds = Context { frontDigits = []
                        , rearDigits = ds
                        , digitContext = undefined
                        , treeContext = []
                        }

updateListCtx ctx d = Context { frontDigits = d : frontDigits ctx
                              , rearDigits = Prelude.tail (rearDigits ctx)
                              , digitContext = digitContext ctx
                              , treeContext = treeContext ctx
                              }

walkDigits :: Ord a => Int -> [Digit a] -> Int -> Context a -> (Tree a, Context a)
walkDigits i [] _ _ = error "index out of bounds"
walkDigits i (Digit1 t1 : ds) n ctx
  | i < n * 1 = walkTree (i - n * 0) t1 n (ctx { digitContext = Digit1Go1 })
  | otherwise = walkDigits (i - n * 1) ds (n * 4) (updateListCtx ctx (Digit1 t1))
walkDigits i (Digit2 t1 t2 : ds) n ctx
  | i < n * 1 = walkTree (i - n * 0) t1 n (ctx { digitContext = Digit2Go1 t2 })
  | i < n * 2 = walkTree (i - n * 1) t2 n (ctx { digitContext = Digit2Go2 t1 })
  | otherwise = walkDigits (i - n * 2) ds (n * 4) (updateListCtx ctx (Digit2 t1 t2))
walkDigits i (Digit3 t1 t2 t3 : ds) n ctx
  | i < n * 1 = walkTree (i - n * 0) t1 n (ctx { digitContext = Digit3Go1 t2 t3 })
  | i < n * 2 = walkTree (i - n * 1) t2 n (ctx { digitContext = Digit3Go2 t1 t3 })
  | i < n * 3 = walkTree (i - n * 2) t3 n (ctx { digitContext = Digit3Go3 t1 t2 })
  | otherwise = walkDigits (i - n * 3) ds (n * 4) (updateListCtx ctx (Digit3 t1 t2 t3))
walkDigits i (Digit4 t1 t2 t3 t4 : ds) n ctx
  | i < n * 1 = walkTree (i - n * 0) t1 n (ctx { digitContext = Digit4Go1 t2 t3 t4 })
  | i < n * 2 = walkTree (i - n * 1) t2 n (ctx { digitContext = Digit4Go2 t1 t3 t4 })
  | i < n * 3 = walkTree (i - n * 2) t3 n (ctx { digitContext = Digit4Go3 t1 t2 t4 })
  | i < n * 4 = walkTree (i - n * 3) t4 n (ctx { digitContext = Digit4Go4 t1 t2 t3 })
  | otherwise = walkDigits (i - n * 4) ds (n * 4) (updateListCtx ctx (Digit4 t1 t2 t3 t4))

walkTree :: Ord a => Int -> Tree a -> Int -> Context a -> (Tree a, Context a)
walkTree 0 (Leaf v) _ ctx = (Leaf v, ctx)
walkTree i (Node t1 t2 t3 t4) n ctx
  | i < m * 1 = walkTree (i - m * 0) t1 m (ctx { treeContext = (GoDown1 t2 t3 t4) : treeContext ctx })
  | i < m * 2 = walkTree (i - m * 1) t2 m (ctx { treeContext = (GoDown2 t1 t3 t4) : treeContext ctx })
  | i < m * 3 = walkTree (i - m * 2) t3 m (ctx { treeContext = (GoDown3 t1 t2 t4) : treeContext ctx })
  | otherwise = walkTree (i - m * 3) t4 m (ctx { treeContext = (GoDown4 t1 t2 t3) : treeContext ctx })
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

  lookup i (QL ds) = case walkDigits i ds 1 (initialCtx ds) of
    (Leaf v, _) -> v
    _           -> error "walkDigits shouldn't return non-leaf node"

  update i v (QL ds) = QL $ mappend (reverse frontDigits) (newDigit digitCtx : rearDigits)
    where
      (Leaf _, Context frontDigits (_ : rearDigits) digitCtx treeCtx) = walkDigits i ds 1 (initialCtx ds)

      newDigit Digit1Go1 = Digit1 newTree
      newDigit (Digit2Go1 t2) = Digit2 newTree t2
      newDigit (Digit2Go2 t1) = Digit2 t1 newTree
      newDigit (Digit3Go1 t2 t3) = Digit3 newTree t2 t3
      newDigit (Digit3Go2 t1 t3) = Digit3 t1 newTree t3
      newDigit (Digit3Go3 t1 t2) = Digit3 t1 t2 newTree
      newDigit (Digit4Go1 t2 t3 t4) = Digit4 newTree t2 t3 t4
      newDigit (Digit4Go2 t1 t3 t4) = Digit4 t1 newTree t3 t4
      newDigit (Digit4Go3 t1 t2 t4) = Digit4 t1 t2 newTree t4
      newDigit (Digit4Go4 t1 t2 t3) = Digit4 t1 t2 t3 newTree

      newTree = constructTree (Leaf v) treeCtx

      constructTree t [] = t
      constructTree t (GoDown1 t2 t3 t4 : ws) = constructTree (Node t t2 t3 t4) ws
      constructTree t (GoDown2 t1 t3 t4 : ws) = constructTree (Node t1 t t3 t4) ws
      constructTree t (GoDown3 t1 t2 t4 : ws) = constructTree (Node t1 t2 t t4) ws
      constructTree t (GoDown4 t1 t2 t3 : ws) = constructTree (Node t1 t2 t3 t) ws
