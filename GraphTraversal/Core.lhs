> module GraphTraversal.Core
> where

> type PinID  = Int
> type CompID = Int

The fundamental datatype in the structured graph is a node.
Every node has:
  Name          := String
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int
  Format String := String

> data Node
>   = MkNode { name       :: String
>            , sinkPins   :: [PinID]
>            , sourcePins :: [PinID]
>            , compId     :: CompID
>            , fmtStrg    :: String
>            }
            
So the next datatype to be defined is an edge.
The edge knows the pin it comes from, as well as
the pin it goes to.

> data Edge
>   = MkEdge { sourceInfo :: (CompID, PinID)
>            , sinkInfo   :: (CompID, PinID)
>            }
