-- Given a reference of a node in a connected undirected graph, return a deep copy (clone) of the graph. Each node in the graph contains a val (int) and a list (List[Node]) of its neighbors.

module CloneGraph
  (
    cloneGraph
  , Line(..)
  , Graph(..)
  ) where


cloneGraph :: Graph a -> Graph a
cloneGraph = id

data Line a = Line a [a] deriving (Eq, Show)

type Graph a = [Line a]
