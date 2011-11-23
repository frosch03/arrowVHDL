module GraphTraversal.Tools
where

-- import Data.List (union, groupBy, (\\), isInfixOf)
-- import Data.Maybe 
-- import Data.Either
-- import Control.Monad (msum)

-- import GHC.Exts (sortWith)

-- For DemoFunctions
import System.IO (writeFile)
import System.Cmd (system)


import Data.List ((\\))

import GraphTraversal.Core
import GraphTraversal.Show
import GraphTraversal.Graph
import GraphTraversal.Auxillary
import GraphTraversal.Show.DOT

type Node = StructGraph


-- Demo Functions
-----------------
write x    = writeFile "/tmp/test.dot" (GraphTraversal.Show.DOT.showStructGraph x)
genPicture = system "dot /tmp/test.dot -Tjpg -o /tmp/test.jpg"

par1 :: Int
par1 = (0)

par2 :: (Int, Int)
par2 = (0,0)


toNode :: (String, Int, Int) -> StructGraph
toNode (name, inPins, outPins)
    = emptyGraph { name    = name
                 , sinks   = [0..(inPins -1)]
                 , sources = [0..(outPins -1)]
                 }


filterByName :: StructGraph -> String -> StructGraph
filterByName s n 
    = if (name s == n) && (length (nodes s) < 1) 
        then NoSG
        else s { nodes = (map (flip filterByName n) $ nodes s) }


atomic :: StructGraph -> Bool
atomic s = length (nodes s) <= 0

replace :: StructGraph -> (Node, Node) -> StructGraph
replace s ft@(from, to) 
    | not $ atomic s
    = s { nodes = map (flip replace $ ft) (nodes s) }
    
    |  name s             == name from
    && length (sinks   s) == length (sinks   from)
    && length (sources s) == length (sources from)
    && length (sinks   s) == length (sinks   to)
    && length (sources s) == length (sources to)
    = to { compID = compID s }

    | otherwise = s

-- mark / cut / trim

rebuildIf :: (StructGraph -> Bool) ->  ([Edge], [Node]) -> Node -> ([Edge], [Node])
rebuildIf isIt (super_es, new_ns) NoSG = (super_es, new_ns)
rebuildIf isIt (super_es, new_ns) n
    |  isIt n       && length (sinks n) == length (sources n)
    = (new_es , new_ns)

    | otherwise 
    = (super_es, new_ns ++ [n'])

    where new_es  = (super_es \\ (lws ++ rws)) ++ nws
          lws     = leftWires super_es (compID n)
          rws     = rightWires super_es (compID n)
          nws     = zipWith MkEdge (map sourceInfo lws) (map sinkInfo rws)
          (es,ns) = foldl (rebuildIf isIt) (edges n, []) $ nodes n
          n'      = n { nodes = ns
                      , edges = es
                      }

bypass :: StructGraph -> Node -> StructGraph
bypass s item 
  = s { nodes = ns
      , edges = es
      }
  where (es, ns) = foldl (rebuildIf (\x -> name x == name item)) (edges s, []) $ nodes s


grepWires :: (Edge -> Anchor) -> [Edge] -> CompID -> [Edge]
grepWires f es cid = filter (\e -> fst (f e) == Just cid) es

leftWires :: [Edge] -> CompID -> [Edge]
leftWires = grepWires sinkInfo

rightWires :: [Edge] -> CompID -> [Edge]
rightWires = grepWires sourceInfo

solderWires :: ([Edge], [Edge]) -> [Edge]
solderWires = mergeEdges
