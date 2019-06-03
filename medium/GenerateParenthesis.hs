-- Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

module GenerateParenthesis
  (
    generateParenthesis
  ) where

type Parenthesis = String
type Left = Int
type Right = Int

generateParenthesis :: Int -> [Parenthesis]
generateParenthesis n = generateParenthesis' n n

generateParenthesis' :: Left -> Right -> [Parenthesis]
generateParenthesis' 0 n = [replicate n ')']
generateParenthesis' l r
  | l < r =  fmap ('(':) (generateParenthesis' (l - 1) r) `mappend`
             fmap (')':) (generateParenthesis' l (r - 1))
  | otherwise = fmap ('(':) (generateParenthesis' (l - 1) r)
