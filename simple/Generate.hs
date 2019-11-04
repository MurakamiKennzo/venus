-- Given a non-negative integer numRows, generate the first numRows of Pascal's triangle.

-- In Pascal's triangle, each number is the sum of the two numbers directly above it.

module Generate
  (
    generate
  ) where

generate 0 = []
generate 1 = [[1]]
generate 2 = [[1], [1, 1]]
generate a = let b = generate (a - 1) in b ++ [1 : generate' (last b) ++ [1]]
  where generate' :: [Int] -> [Int]
        generate' [] = []
        generate' [_] = []
        generate' (a:c@(b:_)) = a + b : generate' c
