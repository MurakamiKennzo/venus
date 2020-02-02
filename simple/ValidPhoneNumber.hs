-- Given a text file file.txt that contains list of phone numbers (one per line), write a one liner bash script to print all valid phone numbers.

-- You may assume that a valid phone number must appear in one of the following two formats: (xxx) xxx-xxxx or xxx-xxx-xxxx. (x means a digit)

-- You may also assume each line in the text file must not contain leading or trailing white spaces.

{-# LANGUAGE QuasiQuotes #-}

module ValidPhoneNumber
  (
    validPhoneNumber
  ) where

import Data.List ( lines )
import Control.Monad ( forM_ )
import System.Environment ( getArgs )
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )
import Text.RawString.QQ ( r )
import Text.Regex.TDFA ( (=~) )

validPhoneNumber :: IO ()
validPhoneNumber = do
  args <- getArgs
  case args of
    [] -> putStrLn "validPhoneNumber: need one argument"
    (file:_) -> validPhoneNumberIO file `catch` validPhoneNumberError

validPhoneNumberError :: IOError -> IO ()
validPhoneNumberError e
  | isDoesNotExistError e = putStrLn "validPhoneNumber: file does not exist"
  | otherwise = ioError e

validPhoneNumberIO :: FilePath -> IO ()
validPhoneNumberIO filePath = do
  content <- readFile filePath
  forM_ (validPhoneNumber' . lines $ content) putStrLn

validPhoneNumber' :: [String] -> [String]
validPhoneNumber' = filter matchPhoneNumber
  where matchPhoneNumber :: String -> Bool
        matchPhoneNumber = (=~ [r|^(\([0-9]{3}\) |[0-9]{3}-)[0-9]{3}-[0-9]{4}$|])
