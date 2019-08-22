module RestoreIpAddresses
  (
    restoreIpAddresses
  ) where

restoreIpAddresses :: String -> [String]
restoreIpAddresses x = let a = iterate appendChunk (readChunk x)
                       in  map fst . filter isValid $ a !! 3
  where isValid :: (String, String) -> Bool
        isValid (_, "") = True
        isValid _ = False

readChunk :: String -> [(String, String)]
readChunk "" = []
readChunk ('0':_:_) = []
readChunk [x] = [ ([x], "") ]
readChunk (x:y:[]) = [ ([x], [y])
                     , ([x, y], "") ]
readChunk (x:y:z:xs) = let a = [x]
                           b = [x, y]
                           c = [x, y, z]
                           a' = (y:z:xs)
                           b' = (z:xs)
                           c' = xs
                       in  if read c > 255 
                              then [ (a, a')
                                   , (b, b') ] 
                              else [ (a, a')
                                   , (b, b')
                                   , (c, c') ]

appendChunk :: [(String, String)] -> [(String, String)]
appendChunk [] = []
appendChunk xs = do
  (a, b) <- xs
  (c, d) <- readChunk b
  return (a ++ "." ++ c, d)
