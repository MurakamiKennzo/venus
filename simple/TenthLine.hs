-- Given a text file file.txt, print just the 10th line of the file.

module TenthLine
  (
    tenthLine
  ) where

import System.Environment ( getArgs )
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )

tenthLine :: IO ()
tenthLine = do
  args <- getArgs
  case args of
    [] -> putStrLn "tenthLine: need one argument"
    (file:_) -> tenthLineIO file `catch` tenthLineError

tenthLineError :: IOError -> IO ()
tenthLineError e
  | isDoesNotExistError e = putStrLn "tenthLine: file does not exist"
  | otherwise = ioError e

tenthLineIO :: FilePath -> IO ()
tenthLineIO filePath = do
  content <- readFile filePath
  case tenthLine' content of
    Nothing -> return ()
    Just line -> putStrLn line

tenthLine' :: String -> Maybe String
tenthLine' = findTenthLine . lines
  where findTenthLine :: [String] -> Maybe String
        findTenthLine = do
          x <- id
          y <- length
          return $ if y >= 10 then Just $ x!!9 else Nothing
