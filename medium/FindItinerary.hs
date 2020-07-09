-- Given a list of airline tickets represented by pairs of departure and arrival airports [from, to], reconstruct the itinerary in order. All of the tickets belong to a man who departs from JFK. Thus, the itinerary must begin with JFK.

-- Note:

-- If there are multiple valid itineraries, you should return the itinerary that has the smallest lexical order when read as a single string. For example, the itinerary ["JFK", "LGA"] has a smaller lexical order than ["JFK", "LGB"].
-- All airports are represented by three capital letters (IATA code).
-- You may assume all tickets form at least one valid itinerary.
-- One must use all the tickets once and only once.

module FindItinerary
  (
    findItinerary
  ) where

import Data.List ( delete )

findItinerary :: [(String, String)] -> [String]
findItinerary = findItinerary' "JFK"

findItinerary' :: (Eq a, Ord a) => a -> [(a, a)] -> [a]
findItinerary' a [] = [a]
findItinerary' a xs = let b = minimum . filter ((== a) . fst) $ xs
                      in  a: findItinerary' (snd b) (delete b xs)
