-- Given a string of numbers and operators, return all possible results from computing all the different possible ways to group numbers and operators. The valid operators are +, - and *.

module DiffWaysToCompute
  (
    diffWaysToCompute
  ) where

import Data.List ( findIndices )

diffWaysToCompute :: (Num a, Read a) => String -> [a]
diffWaysToCompute = diffWaysToCompute' . filter (/= ' ')

diffWaysToCompute' :: (Num a, Read a) => String -> [a]
diffWaysToCompute' xs = let indices = findIndices isOperator xs
                        in  if null indices 
                              then [read xs] 
                              else concat $ do
                                i <- indices
                                let (a, b:c) = splitAt i xs
                                return $ compute <$> map show (diffWaysToCompute' a) <*> [b] <*> map show (diffWaysToCompute' c)

isOperator :: Char -> Bool
isOperator = do
  a <- (== '+')
  b <- (== '-')
  c <- (== '*')
  return $ a || b || c

compute :: (Num a, Read a) => String -> Char -> String -> a
compute a b c
  | b == '+' = aa + cc
  | b == '-' = aa - cc
  | b == '*' = aa * cc
  where aa = read a
        cc = read c
