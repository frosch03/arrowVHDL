module Grid.Tools
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

import Grid.Core
import Grid.Show
import Grid.Graph
import Grid.Auxillary
import Grid.Show.DOT
import Grid.Workers (mergeEdges)

type Node = CircuitDescriptor


-- Demo Functions
-----------------
write x    = writeFile "/tmp/test.dot" (Grid.Show.DOT.showCircuit x)
genPicture = system "dot /tmp/test.dot -Tjpg -o /tmp/test.jpg"

par1 :: Int
par1 = (0)

par2 :: (Int, Int)
par2 = (0,0)


toCircuit :: (String, Int, Int) -> CircuitDescriptor
toCircuit (name, inPins, outPins)
    = emptyCircuit { label   = name
                 , sinks   = [0..(inPins -1)]
                 , sources = [0..(outPins -1)]
                 }


filterByName :: CircuitDescriptor -> String -> CircuitDescriptor
filterByName s n 
    = if (label s == n) && (length (nodes s) < 1) 
        then NoDescriptor
        else s { nodes = (map (flip filterByName n) $ nodes s) }


atomic :: CircuitDescriptor -> Bool
atomic s = length (nodes s) <= 0

replace :: CircuitDescriptor -> (CircuitDescriptor, CircuitDescriptor) -> CircuitDescriptor
replace s ft@(from, to) 
    | not $ atomic s
    = s { nodes = map (flip replace $ ft) (nodes s) }
    
    |  label s            == label from
    && length (sinks   s) == length (sinks   from)
    && length (sources s) == length (sources from)
    && length (sinks   s) == length (sinks   to)
    && length (sources s) == length (sources to)
    = to { compID = compID s }

    | otherwise = s

-- mark / cut / trim

rebuildIf :: (CircuitDescriptor -> Bool) ->  ([Edge], [CircuitDescriptor]) -> CircuitDescriptor -> ([Edge], [CircuitDescriptor])
rebuildIf isIt (super_es, new_ns) NoDescriptor = (super_es, new_ns)
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

bypass :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
bypass s item 
  = s { nodes = ns
      , edges = es
      }
  where (es, ns) = foldl (rebuildIf (\x -> label x == label item)) (edges s, []) $ nodes s


grepWires :: (Edge -> Anchor) -> [Edge] -> CompID -> [Edge]
grepWires f es cid = filter (\e -> fst (f e) == Just cid) es

leftWires :: [Edge] -> CompID -> [Edge]
leftWires = grepWires sinkInfo

rightWires :: [Edge] -> CompID -> [Edge]
rightWires = grepWires sourceInfo

solderWires :: ([Edge], [Edge]) -> [Edge]
solderWires = mergeEdges
