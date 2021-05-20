-- A password is considered strong if the below conditions are all met:

--   It has at least 6 characters and at most 20 characters.
--   It contains at least one lowercase letter, at least one uppercase letter, and at least one digit.
--   It does not contain three repeating characters in a row (i.e., "...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
-- Given a string password, return the minimum number of steps required to make password strong. if password is already strong, return 0.

-- In one step, you can:

--   Insert one character to password,
--   Delete one character from password, or
--   Replace one character of password with another character.

module StrongPasswordChecker
  (
    strongPasswordChecker
  ) where

import Data.Function ( on )
import Data.List ( insertBy
                 , group
                 , sortOn )

strongPasswordChecker :: String -> Int
strongPasswordChecker xs
  | n < 6 = max a (6 - n)
  | n >= 6 && n <= 20 = max a $ sum $ map (`div` 3) ys
  | n > 20 = let b = n - 20
                 c = sum . map (`div` 3) $ repeatDelete b ys
             in  b + max a c
  | otherwise = 0
  where ys = sortOn (`mod` 3) . filter (>= 3) . map length . group $ xs
        a = missingTypeCount xs
        n = length xs

repeatDelete :: Int -> [Int] -> [Int]
repeatDelete 0 xs = xs
repeatDelete _ [] = []
repeatDelete n (x:xs)
  | x == 3 = repeatDelete (n - 1) xs
  | otherwise = repeatDelete (n - 1) (insertBy (compare `on` (`mod` 3)) (x - 1) xs)

missingTypeCount :: String -> Int
missingTypeCount = count . missingTypeCount' (True, True, True)
  where missingTypeCount' :: (Bool, Bool, Bool) -> String -> (Bool, Bool, Bool)
        missingTypeCount' a@(False, False, False) _ = a
        missingTypeCount' a [] = a
        missingTypeCount' (a, b, c) (x:xs)
          | x `elem` ['a' .. 'z'] = missingTypeCount' (False, b, c) xs
          | x `elem` ['A' .. 'Z'] = missingTypeCount' (a, False, c) xs
          | x `elem` ['0' .. '9'] = missingTypeCount' (a, b, False) xs
          | otherwise = missingTypeCount' (a, b, c) xs

        count :: (Bool, Bool, Bool) -> Int
        count (a, b, c) = (if a then 1 else 0) + (if b then 1 else 0) + (if c then 1 else 0)
