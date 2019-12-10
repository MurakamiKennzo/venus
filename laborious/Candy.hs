-- There are N children standing in a line. Each child is assigned a rating value.

-- You are giving candies to these children subjected to the following requirements:

-- Each child must have at least one candy.
-- Children with a higher rating get more candies than their neighbors.
-- What is the minimum candies you must give?

module Candy
  (
    candy
  ) where

candy :: [Int] -> Int
candy = sum . map snd . candy' InComplete . flip zip (repeat 1)

candy' :: State -> [(Int, Int)] -> [(Int, Int)]
candy' Complete a = a
candy' InComplete a = let (b, c) = adjust a
                      in  candy' b c

adjust :: [(Int, Int)] -> (State, [(Int, Int)])
adjust [] = (Complete, [])
adjust [x] = (Complete, [x])
adjust (x:a@(y:xs))
  | fst x > fst y && snd x <= snd y = (InComplete, (fst x, snd y + 1):y:xs)
  | fst x < fst y && snd x >= snd y = (InComplete, x:(fst y, snd x + 1):xs)
  | otherwise = let (b, c) = adjust a in (b, x:c)

data State = InComplete
           | Complete
