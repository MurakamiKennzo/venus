-- The gray code is a binary numeral system where two successive values differ in only one bit.

-- Given a non-negative integer n representing the total number of bits in the code, print the sequence of gray code. A gray code sequence must begin with 0.

module GrayCode
  (
    grayCode
  ) where

import Control.Applicative (ZipList( ZipList
                                   , getZipList ))

grayCode :: Int -> [Int]
grayCode = map toInt . grayCode'

grayCode' :: Int -> [String]
grayCode' 0 = ["0"]
grayCode' 1 = ["0", "1"]
grayCode' n = let a = grayCode' $ n - 1
              in  map ('0' :) a <> (map ('1' :) . reverse) a

toInt :: String -> Int
toInt a = let b = ZipList . map (read . return) . reverse
              c = ZipList . iterate (* 2)
          in  foldl (+) 0 $ (*) <$> b a <*> c 1 
