-- Given an m x n integer matrix heightMap representing the height of each unit cell in a 2D elevation map, return the volume of water it can trap after raining.

module TrapRainWater
  (
    trapRainWater
  ) where

import Data.List ( find
                 , delete
                 , insert )

trapRainWater :: (Num a, Ord a) => [[a]] -> a
trapRainWater xs = trapRainWater' 0 . getItemInfoList $ xs

trapRainWater' :: (Num a, Ord a) => a -> [ItemInfo a] -> a
trapRainWater' a xs
  | all ignore xs = a
  | otherwise = let x = minimum xs
                    (a', xs') = trapRainWater'' x (a, xs)
                in  trapRainWater' a' xs'

trapRainWater'' :: (Num a, Ord a) => ItemInfo a -> (a, [ItemInfo a]) -> (a, [ItemInfo a])
trapRainWater'' x (a, xs) = let (a', xs') = leftUpdate x . bottomUpdate x . rightUpdate x . topUpdate x $ (a, xs)
                            in  (a', insert x { ignore = True } . delete x $ xs')

topUpdate :: (Num a, Ord a) => ItemInfo a -> (a, [ItemInfo a]) -> (a, [ItemInfo a])
topUpdate (ItemInfo b row col visit ignore) (a, xs) =
  case topItemInfo of
    Nothing -> (a, xs)
    Just info@(ItemInfo b' row' col' visit' ignore') ->  if ignore' then (a, xs) else (if b > b' then a + b - b' else a, insert info { item = if b > b' then b else b', visit = True } . delete info $ xs)
  where topItemInfo = find (\ItemInfo { row = row', col = col' } -> row' == row - 1 && col' == col) xs

rightUpdate :: (Num a, Ord a) => ItemInfo a -> (a, [ItemInfo a]) -> (a, [ItemInfo a])
rightUpdate (ItemInfo b row col visit ignore) (a, xs) =
  case rightItemInfo of
    Nothing -> (a, xs)
    Just info@(ItemInfo b' row' col' visit' ignore') ->  if ignore' then (a, xs) else (if b > b' then a + b - b' else a, insert info { item = if b > b' then b else b', visit = True } . delete info $ xs)
  where rightItemInfo = find (\ItemInfo { row = row', col = col' } -> row' == row && col' == col + 1) xs

bottomUpdate :: (Num a, Ord a) => ItemInfo a -> (a, [ItemInfo a]) -> (a, [ItemInfo a])
bottomUpdate (ItemInfo b row col visit ignore) (a, xs) =
  case bottomItemInfo of
    Nothing -> (a, xs)
    Just info@(ItemInfo b' row' col' visit' ignore') ->  if ignore' then (a, xs) else (if b > b' then a + b - b' else a, insert info { item = if b > b' then b else b', visit = True } . delete info $ xs)
  where bottomItemInfo = find (\ItemInfo { row = row', col = col' } -> row' == row + 1 && col' == col) xs

leftUpdate :: (Num a, Ord a) => ItemInfo a -> (a, [ItemInfo a]) -> (a, [ItemInfo a])
leftUpdate (ItemInfo b row col visit ignore) (a, xs) =
  case leftItemInfo of
    Nothing -> (a, xs)
    Just info@(ItemInfo b' row' col' visit' ignore') ->  if ignore' then (a, xs) else (if b > b' then a + b - b' else a, insert info { item = if b > b' then b else b', visit = True } . delete info $ xs)
  where leftItemInfo = find (\ItemInfo { row = row', col = col' } -> row' == row && col' == col - 1) xs

getItemInfoList :: [[a]] -> [ItemInfo a]
getItemInfoList xs = do
    (row, xs'') <- xs'
    [ ItemInfo x row col (firstVisit (row, col)) False | (col, x) <- xs'' ]
  where xs' = zip [0 ..] . map (zip [0 ..]) $ xs
        rows = length xs
        cols = if null xs then 0 else length $ head xs
        firstVisit :: (Int, Int) -> Bool
        firstVisit (row, col)
          | row == 0 = True
          | col == 0 = True
          | row == rows - 1 = True
          | col == cols - 1 = True
          | otherwise = False
        
data ItemInfo a = ItemInfo { item :: a
                           , row :: Int
                           , col :: Int
                           , visit :: Bool
                           , ignore :: Bool } deriving (Eq, Show)

instance (Ord a) => Ord (ItemInfo a) where
  ItemInfo { item = a, visit = visit, ignore = ignore } `compare` ItemInfo { item = a', visit = visit', ignore = ignore' }
    | ignore == True && ignore' == False = GT
    | ignore' == True && ignore == False = LT
    | visit == True && visit' == False = LT
    | visit' == True && visit == False = GT
    | otherwise = compare a a'
