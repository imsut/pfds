{-# LANGUAGE TemplateHaskell #-}
module TestBinaryRAL where

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import BinaryRAL as B

main :: IO ()
main = $(defaultMainGenerator)

prop_drop :: Int -> Int -> Bool
prop_drop m n
  | m <= 0 || n <= 0 || m > 10000 || n > 10000 = True
  | otherwise = B.validate $ B.drop n bl
  where
    bl = foldr B.cons B.empty [1 .. (m + n)] :: BinaryList Int
