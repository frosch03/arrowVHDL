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
>     show = toNetlist


> toNetlist :: StructGraph -> String
> toNetlist g =  "\n"
>             ++ (show.compID) g
>             ++ "(" ++ (show.name) g ++ "): "
>             ++ (prtInOuts.sinks) g ++ "] "
>             ++ (show.edges) g
>             ++ " [" ++ (prtInOuts.sources) g
>             ++ (showNode.nodes) g
>          where showNode [] = ""
>                showNode n  = concat $ map show n
>                prtInOuts [] = "_"
>                prtInOuts x  = foldl1 (\x y -> x ++ ',':y) $ map (show.snd) $ filter (isNothing.fst) x

toVHDL :: StructGraph -> String
toVHDL g =  "\n"
         ++ "var_" ++ (show.name) g ++ " <= "
         ++ 
         ++
         ++
         ++
         ++

In a VHDL-Sorce file, there are two main sections, that we need to specify 
in order to get working VHDL-Source.

Lets concentrate in this function on the "port"-specification ... 

> toPortSpec :: StructGraph -> String
> toPortSpec g =  "\n"
>              ++ "port( clockpin, resetpin : in std_logic;" ++ "\n"
>              ++ inpins  ++ " : in std_logic;" ++ "\n"
>              ++ outpins ++ " : out std_logic;" ++ "\n"
>              ++ ");" ++ "\n"
>    where inpins  = prtPins $ pins "inpin" sinks 
>          outpins = prtPins $ pins "outpin" sources
>          pins :: String -> (StructGraph -> [Connection]) -> [String]
>          pins s f  = map (\x -> s ++ (show.snd $ x)) $ filter (isNothing.fst) $ f g
>          prtPins x = foldl1 (\x y -> x ++ ", " ++ y) $ x

> toComponentSpec :: StructGraph -> String
> toComponentSpec g =  "\n"
>                   ++ "component " ++ name g 
>                   ++ "\n"
>                   ++     toPortSpec g
>                   ++ "\n"
>                   ++ "end component;"

> toSourceSpec :: StructGraph -> String
> toSourceSpec g =  "\n"
>                ++ "begin"
>                ++ 
>                ++ "end"
>                ++ 
>                ++ 
>                ++ 
