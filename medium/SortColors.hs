Given an array with n objects colored red, white or blue, sort them so that objects of the same color are adjacent, with the colors in the order red, white and blue.

Here, we will use the integers 0, 1, and 2 to represent the color red, white, and blue respectively.

Note: You are not suppose to use the library's sort function for this problem.

module SortColors
  (
    sortColors
  ) where

sortColors :: [Color] -> [Color]
sortColors = sortColors' (0, 0, 0)

sortColors' :: (Int, Int, Int) -> [Color] -> [Color]
sortColors' (x, y, z) [] = replicate x 0 ++ replicate y 1 ++ replicate z 2
sortColors' (x, y, z) (x':xs) = case x' of
                                  0 -> sortColors' (x + 1, y, z) xs
                                  1 -> sortColors' (x, y + 1, z) xs
                                  2 -> sortColors' (x, y, z + 1) xs

type Color = Int
