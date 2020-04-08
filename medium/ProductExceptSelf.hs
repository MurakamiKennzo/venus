-- Given an array nums of n integers where n > 1,  return an array output such that output[i] is equal to the product of all the elements of nums except nums[i].

module ProductExceptSelf
  (
    productExceptSelf
  ) where

productExceptSelf :: [Int] -> [Int]
productExceptSelf = map product . exceptSelf
  where exceptSelf :: [Int] -> [[Int]]
        exceptSelf a = let b = [0 .. length a - 1]
                       in  do
                         n <- b
                         return $ take n a <> drop (succ n) a
