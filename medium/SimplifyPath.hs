-- Given an absolute path for a file (Unix-style), simplify it. Or in other words, convert it to the canonical path.

-- In a UNIX-style file system, a period . refers to the current directory. Furthermore, a double period .. moves the directory up a level. For more information, see: Absolute path vs relative path in Linux/Unix

-- Note that the returned canonical path must always begin with a slash /, and there must be only a single slash / between two directory names. The last directory name (if it exists) must not end with a trailing /. Also, the canonical path must be the shortest string representing the absolute path.

module SimplifyPath
  (
    simplifyPath
  ) where

import Data.List ( intercalate )

simplifyPath :: Path -> Path
simplifyPath = simplifyPath' []

simplifyPath' :: [SinglePath] -> Path -> Path
simplifyPath' s "" = '/':intercalate "/" (reverse s)
simplifyPath' s p = let (c, d) = span ( /= '/') p
                        e = if d == [] then [] else tail d
                    in  simplifyPath' (if c == "" then s else whatPath s c) e


whatPath :: [SinglePath] -> SinglePath -> [SinglePath]
whatPath s "." = s
whatPath [] ".." = []
whatPath (x:xs) ".." = xs
whatPath s p = p:s

type Path = String

type SinglePath = String
