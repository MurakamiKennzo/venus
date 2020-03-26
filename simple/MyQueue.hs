-- Implement the following operations of a queue using stacks.

-- push(x) -- Push element x to the back of queue.
-- pop() -- Removes the element from in front of queue.
-- peek() -- Get the front element.
-- empty() -- Return whether the queue is empty.

module MyQueue
  (
    push
  , pop
  , peek
  , empty
  ) where

import Control.Monad.State ( State
                           , state )

push :: a -> State [a] ()
push a = state $ \s -> ((), s <> [a])

pop :: State [a] a
pop = state $ \s -> (head s, tail s)

peek :: State [a] a
peek = state $ \s -> (head s, s)

empty :: State [a] Bool
empty = state $ \s -> (null s, s)
