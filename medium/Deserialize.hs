-- Given a nested list of integers represented as a string, implement a parser to deserialize it.

-- Each element is either an integer, or a list -- whose elements may also be integers or other lists.

-- Note: You may assume that the string is well-formed:

-- String is non-empty.
-- String does not contain white spaces.
-- String contains only digits 0-9, [, - ,, ].

module Deserialize
  (
    deserialize
  ) where

import Data.Char ( isDigit )
import Control.Applicative ( Alternative( (<|>)
                                        , many
                                        , empty ) )

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item >>= \a ->
  if pred a then return a else empty

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> Nothing
    (x:xs) -> return (x, xs)

char :: Char -> Parser Char
char a = satisfy (a ==)

digit :: Parser Int
digit = (do
  flog <- (return `fmap` char '-') <|> return ""
  num <- many (satisfy isDigit)
  return (flog <> num)
  ) >>= \a -> if length a == 0 || a == "-" then empty else return (read a)

sep :: Parser a -> Parser b -> Parser [b]
sep sepParser parser = do
  x <- parser
  xs <- many (sepParser >> parser)
  return (x:xs)

bracket :: Parser a -> Parser a
bracket parser = do
  char '['
  a <- parser
  char ']'
  return a

instance Functor Parser where
  f `fmap` parser = Parser $ \s ->
    runParser parser s >>= \(a, s) -> return (f a, s)

instance Applicative Parser where
  pure a = Parser $ \s -> return (a, s)
  parser <*> parser' = Parser $ \s -> 
    do
      (f, s') <- runParser parser s
      (a, s'') <- runParser parser' s'
      return (f a, s'')

instance Monad Parser where
  return = pure
  parser >>= f = Parser $ \s ->
    do
      (a, s') <- runParser parser s
      (a', s'') <- runParser (f a) s'
      return (a', s'')

instance Alternative Parser where
  empty = Parser $ return Nothing
  parser <|> parser' = Parser $ \s ->
    case runParser parser s of
      Nothing -> runParser parser' s
      a -> a

parse :: Parser a -> String -> Maybe a
parse parser s = case runParser parser s of
                  Just (a, "") -> return a
                  _ -> Nothing

nestedInteger :: Parser NestedInteger
nestedInteger = integerList <|> integerNumber

integerNumber :: Parser NestedInteger
integerNumber = digit >>= return . IntegerNumber

integerList :: Parser NestedInteger
integerList = bracket (sep (char ',') nestedInteger) >>= return . IntegerList

deserialize :: String -> Maybe NestedInteger
deserialize = parse nestedInteger

data NestedInteger = IntegerNumber Int
                   | IntegerList [NestedInteger] deriving (Show, Eq)
