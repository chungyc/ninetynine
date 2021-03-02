{- |
Description: 'cycles'

Some solutions to "Problems.P82" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P82 (cycles) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Cycles with a given node.
--
-- A cycle is a path in a graph whose first and last vertexes are the same vertex.
-- No other vertexes repeat in the path besides these two.
--
-- Write a function which finds all cycles in the graph which include the given vertex.
cycles :: Vertex -> G -> [[Vertex]]
cycles v g = spread v g (v, [v], Set.empty, Set.empty)

spread :: Vertex -> G -> (Vertex, [Vertex], Set Vertex, Set Edge) -> [[Vertex]]
spread target graph (current, path, visited, passed)
  | path /= [target] && target == current = [path]
  | Set.member edge passed = []
  | Set.member current visited = []
  | otherwise = concat $ Set.map continue $ neighbors current graph
  where continue v = spread target graph (v, path', visited', passed')
        edge = Edge (head path, current)
        path' = current : path
        visited' = Set.insert current visited
        passed' = Set.insert edge passed
