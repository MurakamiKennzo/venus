-- Given a text file file.txt, transpose its content.

-- You may assume that each row has the same number of columns and each field is separated by the ' ' character.

module TransposeFile
  (
    transposeFile
  ) where

import Data.List ( intercalate
                 , transpose )
import Control.Monad ( forM_ )
import System.Environment ( getArgs )
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )

transposeFile :: IO ()
transposeFile = do
  args <- getArgs
  case args of
    [] -> putStrLn "transposeFile: need one argument"
    (file:_) -> transposeFileIO file `catch` transposeFileError

transposeFileError :: IOError -> IO ()
transposeFileError e
  | isDoesNotExistError e = putStrLn "transposeFile: file does not exist"
  | otherwise = ioError e

transposeFileIO :: FilePath -> IO ()
transposeFileIO filePath = do
  content <- readFile filePath
  forM_ (transposeFile' content) putStrLn

transposeFile' :: String -> [String]
transposeFile' = map (intercalate " ") . transpose . map words . lines
