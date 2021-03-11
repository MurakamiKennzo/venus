-- Given N axis-aligned rectangles where N > 0, determine if they all together form an exact cover of a rectangular region.

-- Each rectangle is represented as a bottom-left point and a top-right point. For example, a unit square is represented as ((1, 1), (2, 2)). (coordinate of bottom-left point is (1, 1) and top-right point is (2, 2)).

module IsRectangleCover
  (
    isRectangleCover
  ) where

import Data.List ( delete
                 , sort )

isRectangleCover :: [Rectangle] -> Bool
isRectangleCover rectangles = let (area, points) = foldl calcArea (0, []) rectangles
                                  area' = getArea $ sort points
                              in  area == area'

calcArea :: (Double, [Point]) -> Rectangle -> (Double, [Point])
calcArea (area, points) rectangle = (area + area', foldr addPoint points points')
  where points' = getPoints rectangle
        area' = getArea points'

getPoints :: Rectangle -> [Point]
getPoints ((x, y), (x', y')) = sort [ (x, y)
                                    , (x, y')
                                    , (x', y)
                                    , (x', y') ]

getArea :: [Point] -> Double
getArea [(x, y), (x', y'), (x'', y''), (x''', y''')]
  | x == x' && x'' == x''' && y == y'' && y' == y''' = (y' - y) * (x'' - x')
  | otherwise = 0
getArea _ = 0

addPoint :: Point -> [Point] -> [Point]
addPoint point points = if point `elem` points then point `delete` points else point:points

type Rectangle = (Point, Point)

type Point = (Double, Double)
