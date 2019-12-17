module CopyRandomList
  (
    copyRandomList
  ) where

copyRandomList :: Link a -> Link a
copyRandomList = id

data Link a = Empty | Cons a (Link a) deriving (Eq, Show)
