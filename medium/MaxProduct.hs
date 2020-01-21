-- Given an integer array nums, find the contiguous subarray within an array (containing at least one number) which has the largest product.

module MaxProduct 
  (
    maxProduct
  ) where

import Data.Monoid ( Product(..) )
import Data.List ( inits )

maxProduct :: (Num a, Ord a) => [a] -> a
maxProduct a = let b = filter (not . null) . subList $ a
               in  getProduct . maximum $ do
                 c <- b
                 let d = map Product c
                 return $ foldr mappend mempty d

subList :: (Eq a) => [a] -> [[a]]
subList [] = [[]]
subList a = (tail . inits) a <> (subList $ tail a)
