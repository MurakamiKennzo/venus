-- Given two strings s1 and s2 of the same length, determine if s2 is a scrambled string of s1.

module IsScramble
  (
    isScramble
  ) where

isScramble :: String -> String -> Bool
isScramble [a] [b] = if a == b then True else False
isScramble a b = let c = [1 .. length a - 1]
                 in  or . map (isScramble' a b) $ c

isScramble' :: String -> String -> Int -> Bool
isScramble' a b c = let d = take c a
                        e = drop c a
                        f = take c b
                        g = drop c b
                        f' = reverse . take c . reverse $ b
                        g' = reverse . drop c . reverse $ b
                    in  or [ isScramble d f && isScramble e g
                           , isScramble d f' && isScramble e g' ]
