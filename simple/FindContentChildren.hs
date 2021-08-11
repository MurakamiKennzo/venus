-- Assume you are an awesome parent and want to give your children some cookies. But, you should give each child at most one cookie.

-- Each child i has a greed factor g[i], which is the minimum size of a cookie that the child will be content with; and each cookie j has a size s[j]. If s[j] >= g[i], we can assign the cookie j to the child i, and the child i will be content. Your goal is to maximize the number of your content children and output the maximum number.

module FindContentChildren
  (
    findContentChildren
  ) where

import Data.List ( sort )

findContentChildren :: (Ord a) => [a] -> [a] -> Int
findContentChildren xs ys = findContentChildren' 0 xs' ys'
  where xs' = sort xs
        ys' = sort ys
        findContentChildren' :: (Ord a) => Int ->  [a] -> [a] -> Int
        findContentChildren' n _ [] = n
        findContentChildren' n [] _ = n
        findContentChildren' n a@(x:xs) (y:ys)
          | x <= y = findContentChildren' (succ n) xs ys
          | otherwise = findContentChildren' n a ys
