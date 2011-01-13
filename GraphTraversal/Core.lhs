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

A connection is defined by the tuple of componentID and a pinID
There are two special types of edges, those that come from the
outside into the component (called SinkEdge). And those that go
from the component to the outside (called SourceEdge). 
            
> type Connection = (CompID, PinID)
> type SinkEdge   = Connection
> type SourceEdge = Connection

So the next datatype to be defined is an edge. The edge knows 
the pin/component it comes from, as well as the pin/component
it goes to.

> data Edge
>   = MkEdge { sourceInfo :: SourceEdge
>            , sinkInfo   :: SinkEdge
>            }


The actual Graph is formed from the Nodes and Edges

> data StructGraph
>   = MkSG { getNodes   :: [Node]
>          , getEdges   :: [Edge]
>          , getSinks   :: [SinkEdge]
>          , getSources :: [SourceEdge]
>          }

The following defines a simple 1 Bit multiplexer with the needed parts.

> andNode1 = MkNode { name = "AND", sinkPins = [1, 2], sourcePins = [1], compID = 1 }
> andNode2 = MkNode { name = "AND", sinkPins = [1, 2], sourcePins = [1], compID = 2 }
> notNode  = MkNode { name = "NOT", sinkPins = [1],    sourcePins = [1], compID = 3 }
> orNode   = MkNode { name = "OR",  sinkPins = [1, 2], sourcePins = [1], compID = 4 }

> edge1 = MkEdge { sourceInfo = (3, 1), sinkInfo = (1, 2) }
> edge2 = MkEdge { sourceInfo = (1, 1), sinkInfo = (4, 1) }
> edge3 = MkEdge { sourceInfo = (2, 1), sinkInfo = (4, 2) }

> inEdge1 = (1, 1)
> inEdge2 = (2, 1)
> inEdge3 = (2, 2)

> outEdge1 = (4, 1)

> mux1bit = MkSG { getNodes   = [andNode1, andNode2, notNode, orNode]
>                , getEdges   = [edge1, edge2, edge3]
>                , getSinks   = [inEdge1, inEdge2, inEdge3]
>                , getSources = [outEdge1]
>                }

To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Node- and the Edge datatypes also need to be an instance of Show. 

> instance Show (Node) where
>   show nd =  enclose ""    ":"  (show (compID nd))
>           ++ enclose " I(" ")"  (split (map show (sinkPins nd)))
>           ++ enclose "\""  "\"" (name nd)
>           ++ enclose " O(" ")"  (split (map show (sourcePins nd)))
>       where split [] = ""
>             split s  = foldl1 (\x y -> x ++ ", " ++ y) s
>             enclose l r s = l ++ s ++ r

> instance Show (Edge) where
>   show ed =  "-"
>           ++ enclose ""   "> " (show (sinkInfo   ed))
>           ++ enclose " >" ""   (show (sourceInfo ed))
>           ++ "-"
>       where enclose l r s = l ++ s ++ r

> instance Show (StructGraph) where
>   show g =  "\n"
>          ++ enclose "Sinks:   " "\n" (show (getSinks g))
>          ++ enclose "Nodes:   " "\n" (show (getNodes g))
>          ++ enclose "Edges:   " "\n" (show (getEdges g))
>          ++ enclose "Sources: " ""   (show (getSources g))
>       where enclose l r s = l ++ s ++ r
