-- Given s1, s2, s3, find whether s3 is formed by the interleaving of s1 and s2.

module IsInterleave
  (
    isInterleave
  ) where

isInterleave :: String -> String -> String -> Bool
isInterleave "" "" "" = True
isInterleave "" a b = a == b
isInterleave a "" b = a == b
isInterleave _ _ "" = False
isInterleave a@(x:xs) b@(y:ys) (z:zs) = let road = isInterleave xs b zs
                                            road' = isInterleave a ys zs
                                        in  or [ x == z && road
                                               , y == z && road' ]
