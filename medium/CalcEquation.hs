-- You are given an array of variable pairs equations and an array of real numbers values, where equations[i] = [Ai, Bi] and values[i] represent the equation Ai / Bi = values[i]. Each Ai or Bi is a string that represents a single variable.

-- You are also given some queries, where queries[j] = [Cj, Dj] represents the jth query where you must find the answer for Cj / Dj = ?.

-- Return the answers to all queries. If a single answer cannot be determined, return -1.0.

-- Note: The input is always valid. You may assume that evaluating the queries will not result in division by zero and that there is no contradiction.

module CalcEquation
  (
    calcEquation
  ) where

import Data.List ( findIndices
                 , find )
import Data.Monoid ( First( getFirst ) )

calcEquation :: Equations -> Values -> Queries -> [Maybe Double]
calcEquation xs ys zs = map getFirst . map (calcEquation' xs ys) $ zs
  where xs' = mconcat [ [x, y] | (x, y) <- xs ]

calcEquation' :: Equations -> Values -> (String, String) -> First Double
calcEquation' xs ys (a, b)
  | a == b = return 1
  | otherwise = mconcat r
  where r = do
              i <- findIndices (\(a', b') -> b `elem` [a', b']) xs
              let (a', b') = xs !! i
                  c = if a' == b then 1 / (ys !! i) else ys !! i
              return $ (*) <$> calcEquation' (xs `delete` i) (ys `delete` i) (a, if a' == b then b' else a') <*> return c

delete :: [a] -> Int -> [a]
delete xs n = take n xs <> drop (n + 1) xs

type Equations = [(String, String)]

type Values = [Double]

type Queries = [(String, String)]
