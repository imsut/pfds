{-# LANGUAGE TemplateHaskell #-}
module TestLeftistHeap where

import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.List
import LeftistHeap

main = $(defaultMainGenerator)

prop_R :: [Int] -> Bool
prop_R ns = test' lh $ sort ns
  where
    lh :: LeftistHeap Int
    lh = foldl (\h n -> LeftistHeap.insert n h) empty ns
    
    test' :: (LeftistHeap Int) -> [Int] -> Bool
    test' _ []		= True
    test' h (m:ms) 	= findMin h == m && test' (deleteMin h) ms

prop_L :: [Int] -> Bool
prop_L ns = test' lh $ sort ns
  where
    lh :: LeftistHeap Int
    lh = foldl (\h n -> LeftistHeap.insertL n h) empty ns
    
    test' :: (LeftistHeap Int) -> [Int] -> Bool
    test' _ []		= True
    test' h (m:ms) 	= findMin h == m && test' (deleteMinL h) ms
