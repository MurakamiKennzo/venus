-- Shuffle a set of numbers without duplicates.

module Shuffle
  (
    shuffle
  ) where

import Data.IORef ( newIORef
                  , readIORef
                  , writeIORef )
import System.Random ( randomRIO )
import Control.Monad ( forM_ )

shuffle :: [Int] -> IO [Int]
shuffle xs = do
  let l = length xs
  xs' <- mapM newIORef xs
  forM_ [0 .. l - 1] $ \i -> do
      j <- randomRIO (0, i)
      let ix = xs' !! i
          iy = xs' !! j
      x <- readIORef ix
      y <- readIORef iy
      writeIORef ix y
      writeIORef iy x
  mapM readIORef xs'
