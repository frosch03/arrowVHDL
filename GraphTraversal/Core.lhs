> module GraphTraversal.Core
> where

> import Data.Maybe ( isNothing
>                   , fromJust
>                   )

> type PinID  = Int
> type CompID = Int


The structured graph is the fundamental datatype. It holds information
about its nodes, is identified by an unique id. Additionally the in- 
and out pins are listed as well as connections (edges) between the nodes.
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int

Also a name (for debuggin) is helpfull, but could also stored in a 
lookuptable together with a format string that defines the VHDL-Format
  Name          := String
  Format String := String

It turns out, that a Node actually could hold another StructGraph, that 
defines sub structures. It also could hold no sub structure and has only 
a name and a component id.

> data StructGraph
>   = MkSG { name    :: String
>          , compID  :: CompID
>          , nodes   :: [StructGraph]
>          , edges   :: [Edge]
>          , sinks   :: [SinkEdge]
>          , sources :: [SourceEdge]
>          }

Remember, a Sink is something that takes something  (INPUT)
where a Source is something that produces something (OUTPUT)

So the next datatype to be defined is an edge. The edge knows 
the pin/component it comes from, as well as the pin/component
it goes to.

> data Edge
>   = MkEdge { sourceInfo :: SourceEdge
>            , sinkInfo   :: SinkEdge
>            }

A connection is defined by the tuple of componentID and a pinID
There are two special types of edges, those that come from the
outside into the component (called SinkEdge). And those that go
from the component to the outside (called SourceEdge). 
            
> type Connection = (Maybe CompID, PinID)
> type SinkEdge   = Connection
> type SourceEdge = Connection



To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
>       where prtConnection (cid, pid) = show (fromJust cid, pid)

> instance Show (StructGraph) where
>   show g =  "\n"
>          ++ (show.compID) g
>          ++ "(" ++ (show.name) g ++ "): "
>          ++ (prtInOuts.sinks) g ++ "] "
>          ++ (show.edges) g
>          ++ " [" ++ (prtInOuts.sources) g
>          ++ (showNode.nodes) g
>       where showNode [] = ""
>             showNode n  = concat $ map show n
>             prtInOuts x = foldl1 (\x y -> x ++ ',':y) $ map (show.snd) $ filter (isNothing.fst) x
