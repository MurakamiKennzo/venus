-- Find the total area covered by two rectilinear rectangles in a 2D plane.

-- Each rectangle is defined by its bottom left corner and top right corner as shown in the figure.

module ComputeArea
  (
    computeArea
  , Point(..)
  ) where

computeArea :: (Num a, Ord a) => Rectangle a -> Rectangle a -> a
computeArea aa@(Point (l, b), Point (r, t)) bb@(Point (l', b'), Point (r', t')) = (r - l) * (t - b) + (r' - l') * (t' - b') - computeArea' aa bb

computeArea' :: (Num a, Ord a) => Rectangle a -> Rectangle a -> a
computeArea' a b
  | aa' `inArea` bb' = let (Point (l, b), Point (r, t)) = aa'
                           (Point (l', b'), Point (r', t')) = bb'
                       in  (min r r - max l l') * (min t t' - max b b')
  | otherwise = 0
  where aa' = min a b
        bb' = max a b

inArea :: (Ord a) => Rectangle a -> Rectangle a -> Bool
inArea (Point (l, b), Point (r, t)) (Point (l', b'), Point (r', t')) = and [ l' < r
                                                                           , b' < t
                                                                           , t' > b ]

type Rectangle a = (Point a, Point a)

newtype Point a = Point { getPoint :: (a, a) } deriving (Show, Eq, Ord)
