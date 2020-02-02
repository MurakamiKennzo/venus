-- Write a bash script to calculate the frequency of each word in a text file words.txt.

-- For simplicity sake, you may assume:

-- words.txt contains only lowercase characters and space ' ' characters.
-- Each word must consist of lowercase characters only.
-- Words are separated by one or more whitespace characters.

module WordFrequency
  (
    wordFrequency
  ) where

import Data.List ( sort
                 , group
                 , sortOn )
import Control.Monad ( forM_ )
import System.Environment ( getArgs )
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )

wordFrequency :: IO ()
wordFrequency = do
  args <- getArgs
  case args of
    [] -> putStrLn "WordFrequency: need one argument"
    (file:_) -> wordFrequencyIO file `catch` wordFrequencyIOError

wordFrequencyIOError :: IOError -> IO ()
wordFrequencyIOError e
  | isDoesNotExistError e = putStrLn "WordFrequency: file does not exist"
  | otherwise = ioError e

wordFrequencyIO :: FilePath -> IO ()
wordFrequencyIO filePath = do
  content <- readFile filePath
  let wordFreq = reverse . sortOn snd . wordFrequency' . words $ content
  forM_ wordFreq putWordFreq

putWordFreq :: (String, Int) -> IO ()
putWordFreq (word, freq) = putStrLn $ word <> " " <> show freq

wordFrequency' :: [String] -> [(String, Int)]
wordFrequency' = frequency . group . sort
  where frequency :: [[String]] -> [(String, Int)]
        frequency [] = []
        frequency (x:xs) = (head x, length x) : frequency xs
