-- A message containing letters from A-Z is being encoded to numbers using the following mapping:

-- 'A' -> 1
-- 'B' -> 2
-- ...
-- 'Z' -> 26

-- Given a non-empty string containing only digits, determine the total number of ways to decode it.

module NumDecodings
  (
    numDecodings
  ) where

numDecodings :: String -> Int
numDecodings [] = 1
numDecodings [_] = 1
numDecodings ('1':x:xs) = numDecodings (x:xs) + numDecodings xs
numDecodings ('2':x:xs) = let a = numDecodings (x:xs)
                              b = numDecodings xs
                          in  if read [x] < 7 then a + b else a 
numDecodings (_:xs) = numDecodings xs
