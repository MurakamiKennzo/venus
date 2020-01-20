-- Evaluate the value of an arithmetic expression in Reverse Polish Notation.

-- Valid operators are +, -, *, /. Each operand may be an integer or another expression.

-- Note:

-- Division between two integers should truncate toward zero.
-- The given RPN expression is always valid. That means the expression would always evaluate to a result and there won't be any divide by zero operation.

module EvalRPN
  (
    evalRPN
  ) where

import Control.Monad ( foldM
                     , liftM )

evalRPN :: [String] -> Maybe Int
evalRPN a = do
  [b] <- foldM foldFunction [] a
  return b

foldFunction :: [Int] -> String -> Maybe [Int]
foldFunction (x:y:xs) "+" = return $ y + x : xs
foldFunction (x:y:xs) "-" = return $ y - x : xs
foldFunction (x:y:xs) "*" = return $ y * x : xs
foldFunction (x:y:xs) "/" = return $ y `quot` x : xs 
foldFunction xs a = liftM (:xs) $ readMaybe a

readMaybe :: (Read a) => String -> Maybe a
readMaybe a = case reads a of
                [(b, "")] -> return b
                _ -> Nothing
