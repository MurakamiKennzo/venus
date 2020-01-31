-- Given a list of non negative integers, arrange them such that they form the largest number.

module LargestNumber
  (
    largestNumber
  ) where

import Data.List ( sort )

largestNumber :: [Int] -> String
largestNumber = foldr mappend mempty . map getJigsaw . reverse . sort . map (Jigsaw . show)

newtype Jigsaw = Jigsaw { getJigsaw :: [Char] } deriving (Show, Eq)

instance Ord Jigsaw where
  compare (Jigsaw a) (Jigsaw b) = compareJigsaw a b

compareJigsaw :: String -> String -> Ordering
compareJigsaw [] [] = EQ
compareJigsaw _ [] = GT
compareJigsaw [] _ = LT
compareJigsaw (x:xs) (y:[]) = read [x] `compare` (read [y] :: Int) <>
                                case xs of
                                  [] -> EQ
                                  (z:zs) -> read [z] `compare` (read [x] :: Int) <> compareJigsaw zs [y]
compareJigsaw (x:[]) (y:ys) = read [x] `compare` (read [y] :: Int) <>
                                case ys of
                                  [] -> EQ
                                  (z:zs) -> read [y] `compare` (read [z] :: Int) <> compareJigsaw [x] zs
compareJigsaw (x:xs) (y:ys) = read [x] `compare` (read [y] :: Int) <>
                                compareJigsaw xs ys
