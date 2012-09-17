{-# LANGUAGE TemplateHaskell #-}
module TestSkewTrinaryRAL where

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import SkewTrinaryRAL as S

main :: IO ()
main = $(defaultMainGenerator)

prop_head :: [Int] -> Bool
prop_head [] = True
prop_head ns = Prelude.head ns == S.head ral
  where
    ral :: S.SkewTrinaryRAL Int
    ral = foldr S.cons S.empty ns

prop_head_tail :: [Int] -> Bool
prop_head_tail ns = test' (foldr S.cons S.empty ns) ns
  where
    test' :: S.SkewTrinaryRAL Int -> [Int] -> Bool
    test' _ [] = True
    test' ral (m:ms) = S.head ral == m && test' (S.tail ral) ms

prop_lookup :: [Int] -> Bool
prop_lookup [] = True
prop_lookup ns = test' (foldr S.cons S.empty ns) ns 0
  where
    test' :: S.SkewTrinaryRAL Int -> [Int] -> Int -> Bool
    test' _ [] _ = True
    test' ral (m:ms) i = S.lookup i ral == m && test' ral ms (i + 1)

prop_update :: [Int] -> Int -> Int -> Bool
prop_update [] _ _ = True
prop_update ns val r = test' (S.update idx val $ foldr S.cons S.empty ns) ns 0
  where
    idx = r `mod` length ns

    test' :: S.SkewTrinaryRAL Int -> [Int] -> Int -> Bool
    test' _ [] _ = True
    test' ral (m:ms) i
      | i == idx  = S.lookup i ral == val && test' ral ms (i + 1)
      | otherwise = S.lookup i ral == m   && test' ral ms (i + 1)
