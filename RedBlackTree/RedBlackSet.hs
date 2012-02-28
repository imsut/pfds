{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RedBlackSet (
  module Set,
  RedBlackSet,
  RedBlackSet1,
  RedBlackSet2,
  RedBlackSet3,
  RedBlackSet4,
  RedBlackSet5) where

import Set

data Color = R | B deriving (Show, Eq)

-- default implementation of "Purely Functional Data Structures"
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


-- Exercise 3.10 (a) (P.28)
data RedBlackSet1 a = E1 | T1 Color (RedBlackSet1 a) a (RedBlackSet1 a) deriving Show

lbalance :: Color -> RedBlackSet1 a -> a -> RedBlackSet1 a -> RedBlackSet1 a
lbalance B (T1 R (T1 R a x b) y c) z d = T1 R (T1 B a x b) y (T1 B c z d)
lbalance B (T1 R a x (T1 R b y c)) z d = T1 R (T1 B a x b) y (T1 B c z d)
lbalance color a x b = T1 color a x b

rbalance :: Color -> RedBlackSet1 a -> a -> RedBlackSet1 a -> RedBlackSet1 a
rbalance B a x (T1 R (T1 R b y c) z d) = T1 R (T1 B a x b) y (T1 B c z d)
rbalance B a x (T1 R b y (T1 R c z d)) = T1 R (T1 B a x b) y (T1 B c z d)
rbalance color a x b = T1 color a x b

instance Ord a => Set RedBlackSet1 a where
  empty = E1

  member _ E1   = False
  member x (T1 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T1 B a y b
    where
      ins E1        = T1 R E1 x E1
      ins n@(T1 B l v r)
        | x < v     = lbalance B (ins l) v r
        | x > v     = rbalance B l v (ins r)
        | otherwise = n
      ins n@(T1 R l v r)
        | x < v     = T1 R (ins l) v r
        | x > v     = T1 R l v (ins r)
        | otherwise = n

      T1 _ a y b = ins s


-- Exercise 3.10 (b) (P.29)
data RedBlackSet2 a = E2 | T2 Color (RedBlackSet2 a) a (RedBlackSet2 a) deriving Show

llbalance :: Color -> RedBlackSet2 a -> a -> RedBlackSet2 a -> RedBlackSet2 a
llbalance B (T2 R (T2 R a x b) y c) z d = T2 R (T2 B a x b) y (T2 B c z d)
llbalance color a x b = T2 color a x b

lrbalance :: Color -> RedBlackSet2 a -> a -> RedBlackSet2 a -> RedBlackSet2 a
lrbalance B (T2 R a x (T2 R b y c)) z d = T2 R (T2 B a x b) y (T2 B c z d)
lrbalance color a x b = T2 color a x b

rlbalance :: Color -> RedBlackSet2 a -> a -> RedBlackSet2 a -> RedBlackSet2 a
rlbalance B a x (T2 R (T2 R b y c) z d) = T2 R (T2 B a x b) y (T2 B c z d)
rlbalance color a x b = T2 color a x b

rrbalance :: Color -> RedBlackSet2 a -> a -> RedBlackSet2 a -> RedBlackSet2 a
rrbalance B a x (T2 R b y (T2 R c z d)) = T2 R (T2 B a x b) y (T2 B c z d)
rrbalance color a x b = T2 color a x b

data Modified = LChild | RChild | NoChild

instance Ord a => Set RedBlackSet2 a where
  empty = E2

  member _ E2   = False
  member x (T2 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T2 B a' y b'
    where
      ins E2        = (T2 R E2 x E2, NoChild)
      ins n@(T2 c l v r)
        | x < v     = case ins l of
                        (insl, LChild)  -> (llbalance c insl v r, LChild)
                        (insl, RChild)  -> (lrbalance c insl v r, LChild)
                        (insl, NoChild) -> (T2 c insl v r, LChild)
        | x > v     = case ins r of
                        (insr, LChild)  -> (rlbalance c l v insr, RChild)
                        (insr, RChild)  -> (rrbalance c l v insr, RChild)
                        (insr, NoChild) -> (T2 c l v insr, RChild)
        | otherwise = (n, NoChild)

      (T2 _ a' y b', _) = ins s



-- another approach to Exercise 3.10 (b) (P.29)
data RedBlackSet3 a = E3 | T3 Color (RedBlackSet3 a) a (RedBlackSet3 a) deriving Show

llbalance3 :: Color -> RedBlackSet3 a -> a -> RedBlackSet3 a -> RedBlackSet3 a
llbalance3 B (T3 R (T3 R a x b) y c) z d = T3 R (T3 B a x b) y (T3 B c z d)
llbalance3 color a x b = T3 color a x b

lrbalance3 :: Color -> RedBlackSet3 a -> a -> RedBlackSet3 a -> RedBlackSet3 a
lrbalance3 B (T3 R a x (T3 R b y c)) z d = T3 R (T3 B a x b) y (T3 B c z d)
lrbalance3 color a x b = T3 color a x b

rlbalance3 :: Color -> RedBlackSet3 a -> a -> RedBlackSet3 a -> RedBlackSet3 a
rlbalance3 B a x (T3 R (T3 R b y c) z d) = T3 R (T3 B a x b) y (T3 B c z d)
rlbalance3 color a x b = T3 color a x b

rrbalance3 :: Color -> RedBlackSet3 a -> a -> RedBlackSet3 a -> RedBlackSet3 a
rrbalance3 B a x (T3 R b y (T3 R c z d)) = T3 R (T3 B a x b) y (T3 B c z d)
rrbalance3 color a x b = T3 color a x b

instance Ord a => Set RedBlackSet3 a where
  empty = E3

  member _ E3   = False
  member x (T3 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T3 B a' y b'
    where
      ins E3        = T3 R E3 x E3
      ins n@(T3 c E3 v E3)
        | x < v     = T3 c (ins E3) v E3
        | x > v     = T3 c E3 v (ins E3)
        | otherwise = n
      ins n@(T3 c l@(T3 _ _ lv _) v E3)
        | x < lv    = llbalance3 c (ins l) v E3
        | x < v     = lrbalance3 c (ins l) v E3
        | x > v     = T3 c l v (ins E3)
        | otherwise = n
      ins n@(T3 c E3 v r@(T3 _ _ rv _))
        | x < v     = T3 c (ins E3) v r
        | x < rv    = rlbalance3 c E3 v (ins r)
        | x > rv    = rrbalance3 c E3 v (ins r)
        | otherwise = n
      ins n@(T3 c l@(T3 _ _ lv _) v r@(T3 _ _ rv _))
        | x < lv    = llbalance3 c (ins l) v r
        | x < v     = lrbalance3 c (ins l) v r
        | x < rv    = rlbalance3 c l v (ins r)
        | x > rv    = rrbalance3 c l v (ins r)
        | otherwise = n

      T3 _ a' y b' = ins s


-- Exercise 3.10 (b) (P.29)
data RedBlackSet4 a = E4 | T4 Color (RedBlackSet4 a) a (RedBlackSet4 a) deriving Show

instance Ord a => Set RedBlackSet4 a where
  empty = E4

  member _ E4   = False
  member x (T4 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = T4 B a' y b'
    where
      ins E4        = (T4 R E4 x E4, NoChild, Nothing, Nothing)
      ins n@(T4 B l v r)
        | x < v     = case ins l of
                        (T4 R (T4 R a x b) y c, LChild, Just R, Just R) ->
                          (T4 R (T4 B a x b) y (T4 B c v r), LChild, Just R, Just B)
                        (T4 R a x (T4 R b y c), RChild, Just R, Just R) ->
                          (T4 R (T4 B a x b) y (T4 B c v r), LChild, Just R, Just B)
                        (insl@(T4 cc _ _ _), _, _, _)                   ->
                          (T4 B insl v r, LChild, Just B, Just cc)
        | x > v     = case ins r of
                        (T4 R (T4 R a x b) y c, LChild, Just R, Just R) ->
                          (T4 R (T4 B l v a) x (T4 B b y c), RChild, Just R, Just B)
                        (T4 R a x (T4 R b y c), RChild, Just R, Just R) ->
                          (T4 R (T4 B l v a) x (T4 B b y c), RChild, Just R, Just B)
                        (insr@(T4 cc _ _ _), _, _, _)                   ->
                          (T4 B l v insr, RChild, Just B, Just cc)
        | otherwise = (n, NoChild, Nothing, Nothing)
      ins n@(T4 R l v r)
        | x < v     = case ins l of
                        (insl@(T4 cc _ _ _), _, _, _)                   ->
                          (T4 R insl v r, LChild, Just R, Just cc)
        | x > v     = case ins r of
                        (insr@(T4 cc _ _ _), _, _, _)                   ->
                          (T4 R l v insr, RChild, Just R, Just cc)
        | otherwise = (n, NoChild, Nothing, Nothing)

      (T4 _ a' y b', _, _, _) = ins s

-- Exercise 3.10 (b) (P.29)
data RedBlackSet5 a = E5 | T5 Color (RedBlackSet5 a) a (RedBlackSet5 a) deriving Show

llbalance5 :: Color -> RedBlackSet5 a -> a -> RedBlackSet5 a -> RedBlackSet5 a
llbalance5 B (T5 R (T5 R a x b) y c) z d = T5 R (T5 B a x b) y (T5 B c z d)
llbalance5 color a x b = T5 color a x b

lrbalance5 :: Color -> RedBlackSet5 a -> a -> RedBlackSet5 a -> RedBlackSet5 a
lrbalance5 B (T5 R a x (T5 R b y c)) z d = T5 R (T5 B a x b) y (T5 B c z d)
lrbalance5 color a x b = T5 color a x b

rlbalance5 :: Color -> RedBlackSet5 a -> a -> RedBlackSet5 a -> RedBlackSet5 a
rlbalance5 B a x (T5 R (T5 R b y c) z d) = T5 R (T5 B a x b) y (T5 B c z d)
rlbalance5 color a x b = T5 color a x b

rrbalance5 :: Color -> RedBlackSet5 a -> a -> RedBlackSet5 a -> RedBlackSet5 a
rrbalance5 B a x (T5 R b y (T5 R c z d)) = T5 R (T5 B a x b) y (T5 B c z d)
rrbalance5 color a x b = T5 color a x b

instance Ord a => Set RedBlackSet5 a where
  empty = E5

  member _ E5   = False
  member x (T5 _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x E5 = T5 B E5 x E5
  insert x n@(T5 _ l v r)
    | x < v     = let T5 _ a y b = insl n in T5 B a y b
    | x > v     = let T5 _ a y b = insr n in T5 B a y b
    | otherwise = n
      where
        insl (T5 color E5 v r) = T5 color (T5 R E5 x E5) v r
        insl n@(T5 color l@(T5 _ l2 v2 r2) v r)
          | x < v2 = llbalance5 color (insl l) v r
          | x > v2 = lrbalance5 color (insr l) v r
          | otherwise = n
        insr (T5 color l v E5) = T5 color l v (T5 R E5 x E5)
        insr n@(T5 color l v r@(T5 _ l2 v2 r2))
          | x < v2 = rlbalance5 color l v (insl r)
          | x > v2 = rrbalance5 color l v (insr r)
          | otherwise = n

st0 :: RedBlackSet Int
st0 = foldr insert empty $ reverse [1..63] :: RedBlackSet Int

st1 :: RedBlackSet1 Int
st1 = foldr insert empty $ reverse [1..63] :: RedBlackSet1 Int

st2 :: RedBlackSet2 Int
st2 = foldr insert empty $ reverse [1..63] :: RedBlackSet2 Int

st3 :: RedBlackSet3 Int
st3 = foldr insert empty $ reverse [1..63] :: RedBlackSet3 Int

st4 :: RedBlackSet4 Int
st4 = foldr insert empty $ reverse [1..63] :: RedBlackSet4 Int

st5 :: RedBlackSet5 Int
st5 = foldr insert empty $ reverse [1..63] :: RedBlackSet5 Int

