{-# LANGUAGE TemplateHaskell #-}
module TestQuaternaryRAL where

import Data.List (sort)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import QuaternaryRAL as Q

main :: IO ()
main = $(defaultMainGenerator)

prop_head :: [Int] -> Bool
prop_head [] = True
prop_head ns = Prelude.head ns == Q.head ral
  where
    ral :: QuaternaryRAL Int
    ral = foldr Q.cons Q.empty ns

prop_tail :: [Int] -> Bool
prop_tail [] = True
prop_tail ns = case Q.tail ral of
  QL [] -> True
  QL (Digit1 (Leaf _) : _) -> True
  QL (Digit2 (Leaf _) _ : _) -> True
  QL (Digit3 (Leaf _) _ _ : _) -> True
  QL (Digit4 (Leaf _) _ _ _ : _) -> True
  _ -> False
  where
    ral :: Q.QuaternaryRAL Int
    ral = foldr Q.cons Q.empty ns

prop_head_tail :: [Int] -> Bool
prop_head_tail ns = test' ral ns
  where
    ral :: Q.QuaternaryRAL Int
    ral = foldr Q.cons Q.empty ns

    test' :: Q.QuaternaryRAL Int -> [Int] -> Bool
    test' _ [] = True
    test' ral (m:ms) = Q.head ral == m && test' (Q.tail ral) ms

prop_lookup :: [Int] -> Bool
prop_lookup [] = True
prop_lookup ns = test' ral ns 0
  where
    ral :: Q.QuaternaryRAL Int
    ral = foldr Q.cons Q.empty ns

    test' _ [] _ = True
    test' ral (m:ms) i = Q.lookup i ral == m && test' ral ms (i + 1)

prop_update :: [Int] -> Int -> Int -> Bool
prop_update [] _ _ = True
prop_update ns val r = test' ral ns 0
  where
    ral :: Q.QuaternaryRAL Int
    ral = Q.update idx val $ foldr Q.cons Q.empty ns

    idx = r `mod` length ns

    test' _ [] _ = True
    test' ral (m:ms) i
      | i == idx  = Q.lookup i ral == val && test' ral ms (i + 1)
      | otherwise = Q.lookup i ral == m   && test' ral ms (i + 1)
