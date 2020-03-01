-- There are a total of n courses you have to take, labeled from 0 to n-1.

-- Some courses may have prerequisites, for example to take course 0 you have to first take course 1, which is expressed as a pair: [0,1]

-- Given the total number of courses and a list of prerequisite pairs, return the ordering of courses you should take to finish all courses.

-- There may be multiple correct orders, you just need to return one of them. If it is impossible to finish all courses, return an empty array.

module FindOrder
  (
    findOrder
  ) where

import Data.Maybe ( fromMaybe )
import Data.List ( find
                 , delete )

findOrder :: Int -> [(Course, Course)] -> [Course]
findOrder n pres = fromMaybe [] $ findOrder' [0 .. pred n] pres

findOrder' :: [Course] -> [(Course, Course)] -> Maybe [Course]
findOrder' [] _ = Just []
findOrder' courses pres = do
  course <- learnCourse courses pres
  let courses' = delete course courses
      pres' = filter ((== course) . fst) pres
  courses'' <- findOrder' courses' pres'
  return $ course : courses''

learnCourse :: [Course] -> [(Course, Course)] -> Maybe Course
learnCourse courses pres = find canLearnCourse courses
  where canLearnCourse :: Course -> Bool
        canLearnCourse course = let pres' = map snd . filter ((== course) . fst) $ pres
                                in  all (`notElem` courses) pres'

type Course = Int
