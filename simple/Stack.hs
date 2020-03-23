-- Implement the following operations of a stack using queues.

-- push(x) -- Push element x onto stack.
-- pop() -- Removes the element on top of the stack.
-- top() -- Get the top element.
-- empty() -- Return whether the stack is empty.

module Stack
  (
    push
  , pop
  , top
  , empty
  ) where

import Control.Monad.State ( State
                           , state )

push :: a -> State [a] ()
push a = state $ \s -> ((), a:s)

pop :: State [a] a
pop = state $ \s -> (head s, tail s)

top :: State [a] a
top = state $ \s -> (head s, s)

empty :: State [a] Bool
empty = state $ \s -> (null s, s)
