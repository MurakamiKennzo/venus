module IsMatch
  (
    isMatch
  ) where

type RegExp = String

dot :: Char
dot = '.'

star :: Char
star = '*'

isMatch :: String -> RegExp -> Bool
isMatch [] [] = True
isMatch _ [] = False
isMatch [] p = if even . length $ p then emptyPattern p else False
isMatch s p
  | length p == 1 = (s == p || p == [dot]) && isMatch (tail s) []
  | p !! 1 == star = isMatch s (drop 2 p) || 
      let heads = head s
          tails = tail s
          headp = head p
      in  (headp == dot || heads == headp) && isMatch tails p
  | otherwise = 
      let heads = head s
          tails = tail s
          headp = head p
          tailp = tail p
      in  (headp == dot || heads == headp) && isMatch tails tailp

emptyPattern :: RegExp -> Bool
emptyPattern x = all (star /=) (evens x) && all (star ==) (odds x)

evens :: String -> String
evens [] = []
evens (x:xs) = x:odds xs

odds :: String -> String
odds [] = []
odds (_:xs) = evens xs
