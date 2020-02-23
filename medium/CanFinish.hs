-- There are a total of n courses you have to take, labeled from 0 to n-1.

-- Some courses may have prerequisites, for example to take course 0 you have to first take course 1, which is expressed as a pair: [0,1]

-- Given the total number of courses and a list of prerequisite pairs, is it possible for you to finish all courses?

module CanFinish
  (
    canFinish
  ) where

import Data.List ( delete
                 , find )

canFinish :: Int -> [(Course, Course)] -> Bool
canFinish n cond = canFinish' [0 .. n - 1] cond

canFinish' :: [Course] -> [(Course, Course)] -> Bool
canFinish' [] _ = True
canFinish' courses cond = let mCourse = canLearnCourse courses cond
                          in  case mCourse of
                                Nothing -> False
                                Just course -> canFinish' (delete course courses) (filter ((/= course) . fst) cond)

canLearnCourse :: [Course] -> [(Course, Course)] -> Maybe Course
canLearnCourse courses cond = find canLearn courses
  where canLearn :: Course -> Bool
        canLearn = do
          a <- (`notElem` (map fst cond))
          b <- \c -> any (`notElem` courses) . map snd $ filter ((== c) . fst) cond
          return $ a || b

type Course = Int
