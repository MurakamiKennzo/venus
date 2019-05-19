module Convert 
  (
    convert
  ) where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Char (isSpace, chr)

separator :: Char
separator = chr 0

convert :: String -> Int -> String
convert [] _ = []
convert s r 
  | r <= 1 = s
  | otherwise = filter (not . (separator ==)) . foldl1 (++) . transpose $ convert' (S.fromList s) 0 r 

convert' :: S.Seq Char -> Int -> Int -> [String]
convert' S.Empty _ _ = []
convert' s i r
  | m == 0 = 
      let (f, l) = S.splitAt r s
      in  toList f : convert' l (i + 1) r
  | otherwise = 
      let f = S.reverse $ S.update m (s `S.index` 0) $ S.replicate r separator
      in  toList f : convert' (S.drop 1 s) (i + 1) r
  where m = i `mod` (r - 1)
