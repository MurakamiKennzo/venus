module FourSum
  (
    fourSum
  ) where

import Control.Monad (guard)

type Four = (Int, Int, Int, Int)
type Three = (Int, Int, Int)
type Two = (Int, Int)

fourSum :: [Int] -> Int -> [Four]
fourSum xs sum = do
  x <- fours xs
  guard $ add4 x == sum
  return x

add4 :: Four -> Int
add4 (a, b, c, d) = a + b + c + d

fours :: [Int] -> [Four]
fours [w, x, y, z] = [(w, x, y, z)]
fours (x:xs) = (map (x >=:::) . threes $ xs) `mappend`
               fours xs

threes :: [Int] -> [Three]
threes [x, y, z] = [(x, y, z)]
threes (x:xs) = (map (x >=::) . twos $ xs) `mappend`
                threes xs

twos :: [Int] -> [Two]
twos [x, y] = [(x, y)]
twos (x:xs) = (do
  y <- xs
  return (x, y) ) `mappend`
  twos xs

infixr 1 >=:::
(>=:::) :: Int -> Three -> Four
a >=::: (x, y, z) = (a, x, y, z)

infixr 1 >=::
(>=::) :: Int -> Two -> Three
a >=:: (x, y) = (a, x, y)
