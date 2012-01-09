{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RedBlackSet (module Set, RedBlackSet, RedBlackSet', RedBlackSet'') where

import Set

data Color = R | B deriving Show
data RedBlackSet a = E0 | T0 Color (RedBlackSet a) a (RedBlackSet a) deriving Show

balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance B (T0 R (T0 R a x b) y c) z d = T0 R (T0 B a x b) y (T0 B c z d)
balance B (T0 R a x (T0 R b y c)) z d = T0 R (T0 B a x b) y (T0 B c z d)
balance B a x (T0 R (T0 R b y c) z d) = T0 R (T0 B a x b) y (T0 B c z d)
balance B a x (T0 R b y (T0 R c z d)) = T0 R (T0 B a x b) y (T0 B c z d)
balance color a x b = T0 color a x b

instance Ord a => Set RedBlackSet a where
  empty = E0

  member _ E0 = False
  member x (T0 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T0 B a y b
    where
      ins E0 = T0 R E0 x E0
      ins n@(T0 color l v r)
        | x < v     = balance color (ins l) v r
        | x > v     = balance color l v (ins r)
        | otherwise = n

      T0 _ a y b = ins s


data RedBlackSet' a = E1 | T1 Color (RedBlackSet' a) a (RedBlackSet' a) deriving Show

lbalance :: Color -> RedBlackSet' a -> a -> RedBlackSet' a -> RedBlackSet' a
lbalance B (T1 R (T1 R a x b) y c) z d = T1 R (T1 B a x b) y (T1 B c z d)
lbalance B (T1 R a x (T1 R b y c)) z d = T1 R (T1 B a x b) y (T1 B c z d)
lbalance color a x b = T1 color a x b

rbalance :: Color -> RedBlackSet' a -> a -> RedBlackSet' a -> RedBlackSet' a
rbalance B a x (T1 R (T1 R b y c) z d) = T1 R (T1 B a x b) y (T1 B c z d)
rbalance B a x (T1 R b y (T1 R c z d)) = T1 R (T1 B a x b) y (T1 B c z d)
rbalance color a x b = T1 color a x b

instance Ord a => Set RedBlackSet' a where
  empty = E1

  member _ E1 = False
  member x (T1 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T1 B a y b
    where
      ins E1 = T1 R E1 x E1
      ins n@(T1 color l v r)
        | x < v     = lbalance color (ins l) v r
        | x > v     = rbalance color l v (ins r)
        | otherwise = n

      T1 _ a y b = ins s


data RedBlackSet'' a = E2 | T2 Color (RedBlackSet'' a) a (RedBlackSet'' a) deriving Show

{-
llbalance :: Color -> RedBlackSet'' a -> a -> RedBlackSet'' a -> RedBlackSet'' a
llbalance B (T2 R (T2 R a x b) y c) z d = T2 R (T2 B a x b) y (T2 B c z d)
llbalance color a x b = T2 color a x b

lrbalance :: Color -> RedBlackSet'' a -> a -> RedBlackSet'' a -> RedBlackSet'' a
lrbalance B (T2 R a x (T2 R b y c)) z d = T2 R (T2 B a x b) y (T2 B c z d)
lrbalance color a x b = T2 color a x b

rlbalance :: Color -> RedBlackSet'' a -> a -> RedBlackSet'' a -> RedBlackSet'' a
rlbalance B a x (T2 R (T2 R b y c) z d) = T2 R (T2 B a x b) y (T2 B c z d)
rlbalance color a x b = T2 color a x b

rrbalance :: Color -> RedBlackSet'' a -> a -> RedBlackSet'' a -> RedBlackSet'' a
rrbalance B a x (T2 R b y (T2 R c z d)) = T2 R (T2 B a x b) y (T2 B c z d)
rrbalance color a x b = T2 color a x b
-}

instance Ord a => Set RedBlackSet'' a where
  empty = E2

  member _ E2 = False
  member x (T2 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T2 B a' y b'
    where
      ins E2       = T2 R E2 x E2
      ins n@(T2 color l v r)
        | x < v     = let insl = ins l
                      in case insl of
                        (T2 R (T2 R a z b) w c) -> T2 R (T2 B a z b) w (T2 B c v r)
                        (T2 R a z (T2 R b w c)) -> T2 R (T2 B a z b) w (T2 B c v r)
                        _                       -> T2 color insl v r
        | x > v     = let insr = ins r
                      in case insr of
                        (T2 R (T2 R b z c) w d) -> T2 R (T2 B l v b) z (T2 B c w d)
                        (T2 R b z (T2 R c w d)) -> T2 R (T2 B l v b) z (T2 B c w d)
                        _                       -> T2 color l v insr
        | otherwise = n

      T2 _ a' y b' = ins s

{-
st0 :: RedBlackSet Int
st0 = foldr (\e s -> insert e s) empty [1..100] :: RedBlackSet Int

st1 :: RedBlackSet' Int
st1 = foldr (\e s -> insert e s) empty [1..100] :: RedBlackSet' Int

st2 :: RedBlackSet'' Int
st2 = foldr (\e s -> insert e s) empty [1..100] :: RedBlackSet'' Int
-}
