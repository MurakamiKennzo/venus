-- All DNA is composed of a series of nucleotides abbreviated as A, C, G, and T, for example: "ACGAATTCCG". When studying DNA, it is sometimes useful to identify repeated sequences within the DNA.

-- Write a function to find all the 10-letter-long sequences (substrings) that occur more than once in a DNA molecule.

module FindRepeatedDnaSequences
  (
    findRepeatedDnaSequences
  ) where

import Data.List ( delete
                 , nub )
import Control.Monad ( guard )

findRepeatedDnaSequences :: DNA -> [DNA]
findRepeatedDnaSequences dna = let dnas = fragments dna
                               in  nub $ do
                                    x <- dnas
                                    let y = delete x dnas
                                    guard (x `elem` y)
                                    return x
  where fragments :: DNA -> [DNA]
        fragments a@(x:xs)
          | length a < 10 = []
          | otherwise = take 10 a : fragments xs

type DNA = String
