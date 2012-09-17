{-# LANGUAGE TemplateHaskell #-}
module TestTrinomialHeap where

import Data.List (sort)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import TrinomialHeap

main :: IO ()
main = $(defaultMainGenerator)

prop_insert :: [Int] -> Bool
prop_insert [] = True
prop_insert ns = findMin heap == (head . sort) ns
  where
    heap :: TrinomialHeap Int
    heap = foldr insert empty ns

prop_deleteMin :: [Int] -> Bool
prop_deleteMin ns = test' heap $ sort ns
  where
    heap :: TrinomialHeap Int
    heap = foldr insert empty ns

    test' :: (TrinomialHeap Int) -> [Int] -> Bool
    test' _ [] = True
    test' h (m:ms) = findMin h == m && test' (deleteMin h) ms

-- how to limit the range of argument value?
prop_subtract1 :: Int -> Bool
prop_subtract1 n
  | n > 10000 = True -- too large to test tolerably quick
  | n >= 2    = ds1 == ds2
  | otherwise = True
  where
    TH ds1 = deleteMin $ foldr insert empty [1..n] :: TrinomialHeap Int
    TH ds2 = foldr insert empty [2..n] :: TrinomialHeap Int

(===) :: Ord a => [Digit a] -> [Digit a] -> Bool
[] === [] = True
(Zero : ds1) === (Zero : ds2) = ds1 == ds2
(One _ : ds1) === (One _ : ds2) = ds1 == ds2
(Two _ : ds1) === (Two _ : ds2) = ds1 == ds2
_ === _ = False
