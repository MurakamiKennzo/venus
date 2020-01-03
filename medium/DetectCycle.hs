-- Given a linked list, return the node where the cycle begins. If there is no cycle, return null.

-- To represent a cycle in the given linked list, we use an integer pos which represents the position (0-indexed) in the linked list where tail connects to. If pos is -1, then there is no cycle in the linked list.

-- Note: Do not modify the linked list.

module DetectCycle
  (
    detectCycle
  , Link(..)
  ) where


detectCycle :: Link a -> Int -> Maybe a
detectCycle = (!>)

data Link a = Empty
            | Link a (Link a) deriving (Eq, Show)

infixl 4 !>
(!>) :: Link a -> Int -> Maybe a
Empty !> _ = Nothing
(Link a b) !> c
  | c < 0 = Nothing
  | c == 0 = Just a
  | otherwise = b !> (pred c)
