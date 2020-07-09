-- One way to serialize a binary tree is to use pre-order traversal. When we encounter a non-null node, we record the node's value. If it is a null node, we record using a sentinel value such as #.

--      _9_
--     /   \
--    3     2
--   / \   / \
--  4   1  #  6
-- / \ / \   / \
-- # # # #   # #
-- For example, the above binary tree can be serialized to the string "9,3,4,#,#,1,#,#,2,#,6,#,#", where # represents a null node.

-- Given a string of comma separated values, verify whether it is a correct preorder traversal serialization of a binary tree. Find an algorithm without reconstructing the tree.

-- Each comma separated value in the string must be either an integer or a character '#' representing null pointer.

-- You may assume that the input format is always valid, for example it could never contain two consecutive commas such as "1,,3".

module IsValidSerialization
  (
    isValidSerialization
  ) where

import Data.List ( elemIndices )

isValidSerialization :: String -> Bool
isValidSerialization = isValidSerialization' "#" . breaks ','

isValidSerialization' :: (Eq a) => a -> [a] -> Bool
isValidSerialization' _ [] = False
isValidSerialization' x [y] = x == y
isValidSerialization' x [_, y, z] = x == y && x == z
isValidSerialization' x (y:ys)
  | x == y = False
  | otherwise = any (isValidSerialization'' x) $ split x ys

isValidSerialization'' :: (Eq a) => a -> ([a], [a]) -> Bool
isValidSerialization'' x (xs, ys) = isValidSerialization' x xs && isValidSerialization' x ys

breaks :: (Eq a) => a -> [a] -> [[a]]
breaks _ [] = []
breaks x xs = a: breaks x c
  where (a, b) = (== x) `break` xs
        c = if null b then [] else tail b

split :: (Eq a) => a -> [a] -> [([a], [a])]
split x xs
  | null indices = [(xs, [])]
  | otherwise = map (flip splitAt xs) indices
  where indices = map succ $ x `elemIndices` xs
