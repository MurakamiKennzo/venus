-- There are two sorted arrays nums1 and nums2 of size m and n respectively.

-- Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

-- You may assume nums1 and nums2 cannot be both empty.

module FindMedianSortedArrays
  (
    SortedList
  , fromList
  , toList
  , findMedianSortedArrays
  ) where

import Data.List (sort)

data SortedList a = SortedList [a] deriving (Show, Eq)

data SortedOrder = Inc | Dec | NoDo

type Length = Int
type Index = Int

fromList :: (Ord a) => [a] -> SortedList a
fromList = SortedList . sort

toList :: SortedList a -> [a]
toList (SortedList x) = x

nth :: Int -> SortedList a -> Maybe a
nth a (SortedList b) = 
  let c = length b
  in  if a >= c || a < 0 then Nothing else Just $ b !! a

findMedianSortedArrays :: (Fractional a, Ord a) => SortedList a -> SortedList a -> Maybe a
findMedianSortedArrays (SortedList a1) (SortedList a2) = 
  let (l1, l2) = (length a1, length a2)
      (s1, s2) = if l1 <= l2 then ((l1, SortedList a1), (l2, SortedList a2)) else ((l2, SortedList a2), (l1, SortedList a1))
      i = 0
  in findMedianSortedArrays' i s1 s2

findMedianSortedArrays' :: (Fractional a, Ord a) => Index -> (Length, SortedList a) -> (Length, SortedList a) -> Maybe a
findMedianSortedArrays' _ (0, SortedList []) (1, SortedList [a]) = Just a
findMedianSortedArrays' i1 (l1, a1) (l2, a2) =
  let l = l1 + l2
      oddl = odd l
      i2 = (l + if oddl then 1 else 0) `div` 2 - i1
      lefta1 = nth (i1 - 1) a1
      lefta2 = nth (i2 - 1) a2
      righta1 = nth i1 a1
      righta2 = nth i2 a2
      compare1 = orderCompare lefta1 righta2
      compare2 = orderCompare lefta2 righta1
      leftNothing = nothing lefta1 lefta2
      rightNothing = nothing righta1 righta2
  in  case doMedian (medianArrays compare1 compare2) leftNothing rightNothing of
        Inc -> findMedianSortedArrays' (i1 + 1) (l1, a1) (l2, a2)
        Dec -> findMedianSortedArrays' (i1 - 1) (l1, a1) (l2, a2)
        NoDo -> if oddl then max lefta1 lefta2 else median (max' lefta1 lefta2) (min' righta1 righta2)

max' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
max' = max

min' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
min' Nothing a = a
min' a Nothing = a
min' a b = min a b

median :: Fractional a => Maybe a -> Maybe a -> Maybe a
median Nothing Nothing = Nothing
median a Nothing = a
median Nothing a = a
median (Just a) (Just b) = Just $ (a + b) / 2

medianArrays :: Ordering -> Ordering -> SortedOrder
medianArrays GT _ = Dec
medianArrays _ GT = Inc
medianArrays _ _ = NoDo

nothing :: Maybe a -> Maybe a -> Bool
nothing Nothing Nothing = True
nothing _ _ = False

doMedian :: SortedOrder -> Bool -> Bool -> SortedOrder
doMedian NoDo True False = Inc
doMedian NoDo False True = Dec
doMedian x _ _ = x

orderCompare :: (Ord a) => Maybe a -> Maybe a -> Ordering
orderCompare _ Nothing = LT
orderCompare a b = compare a b
