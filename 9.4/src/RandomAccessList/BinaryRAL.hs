module BinaryRAL
       (
         module RandomAccessList
       , drop
       , BinaryList

         -- debug
       , validate
       , dropNode
       , toDigitList
       , Tree(..)
       ) where

import Prelude hiding (head, tail, lookup, drop)
import qualified Prelude as P (head)
import RandomAccessList

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = Zero | One (Tree a) deriving Show
newtype BinaryList a = BL [Digit a] deriving Show

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t [] = [One t]
consTree t (Zero : ts) = One t : ts
consTree t1 (One t2 : ts) = Zero : consTree (link t1 t2) ts

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (One t : ts) = (t, Zero : ts)
unconsTree (Zero : ts) = (t1, One t2 : ts')
  where
    (Node _ t1 t2, ts') = unconsTree ts

-- Exercise 9.1
drop :: Int -> BinaryList a -> BinaryList a
drop num (BL trees) = BL $ dropTree num trees
  where
    dropTree :: Int -> [Digit a] -> [Digit a]
    dropTree _ [] = error "no trees left"
    dropTree n (Zero : ts) = dropTree n ts
    dropTree n (One t : ts)
      | n > size t = dropTree (n - size t) ts
      | otherwise  = prune $ toDigitList (length trees - length ts) (dropNode n t []) ++ ts

    prune = reverse . dropWhile isZero . reverse

dropNode :: Int -> Tree a -> [Tree a] -> [Tree a]
dropNode 0 t acc = t : acc
dropNode 1 (Leaf _) acc = acc
dropNode _ (Leaf _) _ = error "Not enough nodes left in the tree"
dropNode n (Node w t1 t2) acc
  | n > w          = error "Not enough nodes left in the tree"
  | n == w         = acc
  | n >= w `div` 2 = dropNode (n - w `div` 2) t2 acc
  | otherwise      = dropNode n t1 (t2 : acc)

-- e.g. [t0, t1] -> [Zero, One t0, One t1, Zero]
toDigitList :: Int -> [Tree a] -> [Digit a]
toDigitList n trees = reverse $ padWithZero (n - length trees') trees'
  where
    trees' = toDigitList' [] trees

    toDigitList' acc [] = acc
    toDigitList' acc (t : ts)
      | 2 ^ (length acc) > size t  = error "accumulator has too many elements"
      | 2 ^ (length acc) == size t = toDigitList' (One t : acc) ts
      | otherwise                  = toDigitList' (Zero : acc) (t : ts)

    padWithZero 0 ds = ds
    padWithZero m ds = padWithZero (m - 1) (Zero : ds)

isZero :: Digit a -> Bool
isZero Zero = True
isZero _ = False
-- end of Exercise 9.1

-- returns True if i-th (0-based) element/tree in the list has exactly (i+1) nodes
validate :: BinaryList a -> Bool
validate (BL trees) = (not . isZero . P.head . reverse $ trees) && validate' 1 trees
  where
    validate' :: Int -> [Digit a] -> Bool
    validate' _ [] = True
    validate' n (Zero : ts)  = validate' (n * 2) ts
    validate' 1 (One (Leaf _) : ts) = validate' 2 ts
    validate' _ (One (Leaf _) : _) = error "Leaf can exist only at head of the list"
    validate' n (One (Node w _ _) : ts) = n == w && validate' (n * 2) ts

instance RandomAccessList BinaryList where
  empty = BL []
  isEmpty (BL ts) = null ts

  cons x (BL ts) = BL (consTree (Leaf x) ts)
  head (BL ts) = x
    where
      (Leaf x, _) = unconsTree ts
  tail (BL ts) = BL ts'
    where
      (_, ts') = unconsTree ts

  lookup i (BL trees) = look i trees
    where
      look :: Int -> [Digit a] -> a
      look _ [] = error "bad subscript"
      look j (Zero : ts) = look j ts
      look j (One t : ts) = if j < size t
                            then lookTree j t
                            else look (j - size t) ts

      lookTree :: Int -> Tree a -> a
      lookTree 0 (Leaf x) = x
      lookTree _ (Leaf _) = error "bad subscript"
      lookTree j (Node w t1 t2) = if j < w `div` 2
                                  then lookTree j t1
                                  else lookTree (j - w `div` 2) t2

  update i y (BL trees) = BL (upd i trees)
    where
      -- TODO: how to reconcile this type constraint with the one for update
--      upd :: Ord a => Int -> [Digit a] -> [Digit a]
      upd _ [] = error "bad subscript"
      upd j (Zero : ts) = Zero : upd j ts
      upd j (One t : ts) = if j < size t
                           then One (updTree j t) : ts
                           else One t : upd (j - size t) ts

      -- TODO: how to reconcile this type constraint with the one for update
--      updTree :: Int -> Tree a -> Tree a
      updTree 0 (Leaf _) = Leaf y
      updTree _ (Leaf _) = error "bad subscript"
      updTree j (Node w t1 t2) = if j < w `div` 2
                                 then Node w (updTree j t1) t2
                                 else Node w t1 (updTree j t2)
