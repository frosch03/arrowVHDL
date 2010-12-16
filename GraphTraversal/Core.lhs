> module GraphTraversal.Core
> where

> type PinID  = Int
> type CompID = Int


The fundamental datatype in the structured graph is a node.
Every node has:
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int
Also a name for debuggin could be helpfull, but could also 
stored in a lookuptable together with a format string that 
defines the VHDL-Format
  Name          := String
  Format String := String

Remember, a Sink is something that takes something  (INPUT)
where a Source is something that produces something (OUTPUT)

> data Node
>   = MkNode { name       :: String
>            , sinkPins   :: [PinID]
>            , sourcePins :: [PinID]
>            , compID     :: CompID
> --         , fmtStrg    :: String
>            }
            

So the next datatype to be defined is an edge.
The edge knows the pin it comes from, as well as
the pin it goes to.

> data Edge
>   = MkEdge { sourceInfo :: (CompID, PinID)
>            , sinkInfo   :: (CompID, PinID)
>            }


The actual Graph is formed from the Nodes and Edges

> data StructGraph
>   = MkSG { getNodes :: [Node]
>          , getEdges :: [Edge]
>          }

> andNode1 = MkNode { name = "AND", sinkPins = [1, 2], sourcePins = [1], compID = 1 }
> andNode2 = MkNode { name = "AND", sinkPins = [1, 2], sourcePins = [1], compID = 2 }
> notNode  = MkNode { name = "NOT", sinkPins = [1],    sourcePins = [1], compID = 3 }
> orNode   = MkNode { name = "OR",  sinkPins = [1, 2], sourcePins = [1], compID = 4 }

> edge1 = MkEdge { sourceInfo = (3, 1), sinkInfo = (1, 2) }
> edge2 = MkEdge { sourceInfo = (1, 1), sinkInfo = (4, 1) }
> edge3 = MkEdge { sourceInfo = (2, 1), sinkInfo = (4, 2) }

> inEdge1 = MkEdge { sourceInfo = (0, 1), sinkInfo = (1, 1) }
> inEdge2 = MkEdge { sourceInfo = (0, 2), sinkInfo = (2, 1) }
> inEdge3 = MkEdge { sourceInfo = (0, 3), sinkInfo = (2, 2) }

> outEdge1 = MkEdge { sourceInfo = (4, 1), sinkInfo = (0, 1) }

> mux1bit = MkSG { getNodes = [andNode1, andNode2, notNode, orNode]
>                , getEdges = [edge1, edge2, edge3, inEdge1, inEdge2, inEdge3, outEdge1]
>                }
