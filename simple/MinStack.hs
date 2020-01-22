-- Design a stack that supports push, pop, top, and retrieving the minimum element in constant time.

-- push(x) -- Push element x onto stack.
-- pop() -- Removes the element on top of the stack.
-- top() -- Get the top element.
-- getMin() -- Retrieve the minimum element in the stack.

module MinStack
  (
    pop
  , push
  , top
  , getMin
  ) where

import Control.Monad.State ( State
                           , state )

type MinStack a = [a]

push :: (Num a) => a -> State (MinStack a) ()
push x = state $ \xs -> ((), x:xs)

pop :: State (MinStack a) (Maybe a)
pop = state $ \a -> case a of
                      (x:xs) -> (Just x, xs)
                      _ -> (Nothing, a)

top :: State (MinStack a) (Maybe a)
top = state $ \a -> case a of
                      (x:xs) -> (Just x, a)
                      _ -> (Nothing, a)

getMin :: (Ord a) => State (MinStack a) (Maybe a)
getMin = state $ \a -> (if null a then Nothing else Just . minimum $ a, a)
