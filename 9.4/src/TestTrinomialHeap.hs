{-# LANGUAGE TemplateHaskell #-}
module TestTrinomialHeap where

import Data.List (sort)
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import TrinomialHeap

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
