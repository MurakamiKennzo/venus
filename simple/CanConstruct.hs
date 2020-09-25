-- Given an arbitrary ransom note string and another string containing letters from all the magazines, write a function that will return true if the ransom note can be constructed from the magazines ; otherwise, it will return false.

-- Each letter in the magazine string can only be used once in your ransom note.

module CanConstruct
  (
    canConstruct
  ) where

import qualified Data.Map as Map

canConstruct :: String -> String -> Bool
canConstruct a b = flip canConstruct' a $ countMap b

countMap :: String -> Map.Map Char Int
countMap = foldr alter' mempty
  where alter' :: Char -> Map.Map Char Int -> Map.Map Char Int
        alter' c = Map.alter alterCount c

        alterCount :: Maybe Int -> Maybe Int
        alterCount Nothing = Just 1
        alterCount m = fmap succ m

canConstruct' :: Map.Map Char Int -> String -> Bool
canConstruct' map "" = True
canConstruct' map (x:xs) = case a of
                            Nothing -> False
                            Just n -> if n - 1 < 0 then False else canConstruct' (Map.adjust pred x map) xs
  where a = x `Map.lookup` map
