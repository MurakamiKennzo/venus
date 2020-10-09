-- Suppose we have the file system represented in the following picture:

-- We will represent the file system as a string where "\n\t" mean a subdirectory of the main directory, "\n\t\t" means a subdirectory of the subdirectory of the main directory and so on. Each folder will be represented as a string of letters and/or digits. Each file will be in the form "s1.s2" where s1 and s2 are strings of letters and/or digits.

-- For example, the file system above is represented as "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext".

-- Given a string input representing the file system in the explained format, return the length of the longest absolute path to a file in the abstracted file system. If there is no file in the system, return 0.

module LengthLongestPath
  (
    lengthLongestPath
  ) where

import Data.Char ( isSpace )

lengthLongestPath :: String -> Int
lengthLongestPath = maybe 0 id . lengthLongestPath' . pathToFolder . pathToFolder' 0

lengthLongestPath' :: Folder -> Maybe Int
lengthLongestPath' (File name) = return $ length name
lengthLongestPath' (Folder name []) = Nothing
lengthLongestPath' (Folder name folders) = (maximum . map lengthLongestPath' $ folders) >>= return . (length name + 1 +)

pathToFolder :: [HierarchyPath] -> Folder
pathToFolder (x:xs)
  | null xs = filerOrFolder name
  | otherwise = Folder name $ map pathToFolder folders
  where name = snd x
        folders = breakFolders xs

filerOrFolder :: String -> Folder
filerOrFolder s = if '.' `elem` s then File s else Folder s []

breakFolders :: [HierarchyPath] -> [[HierarchyPath]]
breakFolders [] = []
breakFolders (x:xs) = let n = fst x
                          (a, b) = break ((== n) . fst) xs
                      in  (x:a):breakFolders b

pathToFolder' :: Int -> Path -> [HierarchyPath]
pathToFolder' _ "" = []
pathToFolder' n xs = let (a, b) = break (== '\n') xs
                         (c, d) = break (not . isSpace) b
                         e = foldr (\a b -> if a == '\t' then succ b else b) 0 c
                   in  (n, a):pathToFolder' e d

type Path = String
type HierarchyPath = (Int, String)

data Folder = File String
            | Folder String [Folder] deriving (Show, Eq)
