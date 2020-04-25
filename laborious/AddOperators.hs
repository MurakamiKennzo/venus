-- Given a string that contains only digits 0-9 and a target value, return all possibilities to add binary operators (not unary) +, -, or * between the digits so they evaluate to the target value.

module AddOperators
  (
    addOperators
  ) where

import Data.List ( inits
                 , tails
                 , elemIndex )
import Data.Maybe ( catMaybes )

addOperators :: String -> Int -> [String]
addOperators xs a = map (foldr step "") . map fst . filter ((== a) . snd) . map (\x -> (x, calculate x)) . foldMap addOperators' . catMaybes . numbers $ xs
  where step :: Cal -> String -> String
        step a b = show a ++ b

calculate :: [Cal] -> Int
calculate [] = 0
calculate [Cal x] = x
calculate a@(x:y:z:xs) = calculate . maybe (calculate' x y z:xs) id $ do
  i <- Multiply `elemIndex` a
  let m = a !! pred i
      n = a !! succ i
      o = a !! i
  return $ take (i - 1) a <> [calculate' m o n] <> drop (i + 2) a
  where calculate' :: Cal -> Cal -> Cal -> Cal
        calculate' (Cal x) o (Cal y)
          | o == Add = Cal $ x + y
          | o == Subtract = Cal $ x - y
          | o == Multiply = Cal $ x * y

addOperators' :: [Int] -> [[Cal]]
addOperators' [] = []
addOperators' [x] = [[Cal x]]
addOperators' (x:xs) = let a = fmap (Cal x :) [ [Add], [Subtract], [Multiply] ]
                       in  (<>) <$> a <*> addOperators' xs

numbers :: String -> [Maybe [Int]]
numbers xs = let ys = drop 1 . inits $ xs
             in  concat $ do
              y <- ys
              let zs = numbers . drop (length y) $ xs
              return $ combine y zs
  where combine :: String -> [Maybe [Int]] -> [Maybe [Int]]
        combine a@(x:xs) b
          | null b = if c then [Nothing] else [return [read a]]
          | otherwise = map ((:) <$> (if c then Nothing else return . read $ a) <*>) b 
          where c = x == '0' && (not . null $ xs)

data Cal = Cal Int
          | Add 
          | Subtract
          | Multiply deriving (Eq)

instance Show Cal where
  show (Cal a) = show a
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
