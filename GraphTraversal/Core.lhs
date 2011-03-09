> module GraphTraversal.Core
> where

> import Data.Maybe ( isNothing
>                   , fromJust
>                   )
> import Prelude hiding (break)


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
>          , sinks   :: [SinkAnchor]
>          , sources :: [SourceAnchor]
>          }
> --  deriving (Show)

Remember, a Sink is something that takes something  (INPUT)
where a Source is something that produces something (OUTPUT)

So the next datatype to be defined is an edge. The edge knows 
the pin/component it comes from, as well as the pin/component
it goes to.

> data Edge
>   = MkEdge { sourceInfo :: SourceAnchor
>            , sinkInfo   :: SinkAnchor
>            }
> --  deriving (Show)

A connection is defined by the tuple of componentID and a pinID
There are two special types of edges, those that come from the
outside into the component (called SinkAnchor). And those that go
from the component to the outside (called SourceAnchor). 
            
> type AnchorPoint = (Maybe CompID, PinID)
> type SinkAnchor   = AnchorPoint
> type SourceAnchor = AnchorPoint



To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
>       where prtConnection (cid, pid) = show (fromJust cid, pid)

> instance Show (StructGraph) where
> --  show = toVHDL
>     show = toSimpleList


In a VHDL-Sorce file, there are two main sections, that we need to specify 
in order to get working VHDL-Source.


First of all we need a function, that appends a newline character t the 
end of a string. 

> break :: String -> String
> break =  flip (++) "\n"


The function that starts the show-process is the toVHDL-function. Here we 
define the basic structure of a VHDL-SourceCode with an header, the 
entity-definition as well as the component-definition. 
TODO: Add the signal-definition and the port-map-definitions

> toVHDL :: StructGraph -> String
> toVHDL g 
>      = concat $ map break
>      [ ""
>      , vhdl_header
>      , vhdl_entity g 
>      , vhdl_components g
>      , vhdl_signals g
>      , vhdl_portmaps g
>      ]


The VHDL-Header is just some boilerplate-code where library's are imported

> vhdl_header :: String
> vhdl_header 
>      = concat $ map break
>      [ "LIBRARY ieee;"
>      , "USE ieee.std_logic_1164.all;"
>      ]


A VHDL-Entity defines an "interface" to a hardware component. It consists of
a name and of some port-definitions (like what wires go inside and come back out)

> vhdl_entity :: StructGraph -> String
> vhdl_entity g 
>      = concat $ map break
>      [ "ENTITY " ++ name g ++ " IS"
>      , "PORT (" ++ vhdl_port_definition g ++ ");"
>      , "END " ++ name g ++ ";"
>      ]


The VHDL-Component definitions describe the basic interface to the components
that are used inside this new definition. We therefore pick the components 
of which these new component consists. We call this components the level 1 
components, because we descent only one step down in the graph. 

> vhdl_components :: StructGraph -> String
> vhdl_components g 
>      = concat $ map component nodes_level1
>     where nodes_level1 = nodes g
>           component g_level1 = concat $ map break
>                              [ ""
>                              , "COMPONENT " ++ name g_level1
>                              , "PORT (" ++ vhdl_component_ports g_level1 ++ ");"
>                              ] 


With the VHDL-Component-Ports function we generate a nice string with all the
in and output pins, seperated by a comma and a blank. 

> vhdl_component_ports :: StructGraph -> String
> vhdl_component_ports g 
>      = concat $ map break
>      [ name_anchors "in" (sinks g)    ++ " : in  std_logic;" 
>      , name_anchors "out" (sources g) ++ " : out std_logic;"
>      ]


With the VHDL-Port-Definition function we generate a nice string with all the
in and output pins, seperated by a comma and a blank. 

> vhdl_port_definition :: StructGraph -> String
> vhdl_port_definition g 
>      = concat $ map break
>      [ name_anchors "inpin" (sinks g)    ++ " : in  std_logic;"
>      , name_anchors "outpin" (sources g) ++ " : out std_logic;"
>      ]


The VHDL-Signals is the list of inner wires, that are used inside the new component.

> vhdl_signals :: StructGraph -> String
> vhdl_signals g 
>      = concat $ map break
>      [ "SIGNAL " ++ signals ++ ": std_logic" 
>      ]
>     where signals = seperate_with ", " ['i': (show x) | x <- [0 .. length (edges g) -1]]



The VHDL-Portmaps function ... 
TODO: 

> vhdl_portmaps :: StructGraph -> String
> vhdl_portmaps g 
>      = concat $ map break
>      [ "BEGIN"
> --   , 
> --   ,
> --   ,
> --   ,
>      , "END"
>      ]
>     where nodes_level1 = nodes g
>           firstSubNode (n:[]) = n
>           firstSubNode ns     = let minID = foldl1 min $ map compID nodes_level1
>                                 in  head $ filter (\n -> compID n == minID) ns
>           lastSubNode (n:[]) = n
>           lastSubNode ns     = let maxID = foldl1 max $ map compID nodes_level1
>                                in  head $ filter (\n -> compID n == maxID) ns
>           anchors name as = map (\x -> name ++ (show.snd $ x))
>                           $ filter (isNothing.fst)
>                           $ as

The Name-Anchors function takes a string and a list of anchor points. The string is the 
prepended infront of every anchor points number. All the strings are then concated and 
seperated with a comma and a blank. 

> name_anchors :: String -> [AnchorPoint] -> String
> name_anchors name as = seperate_with ", " 
>                      $ map (\x -> name ++ (show.snd $ x)) 
>                      $ filter (isNothing.fst) 
>                      $ as

> seperate_with :: String -> [String] -> String
> seperate_with sep = foldl1 (\x y -> x ++ sep ++ y)





> toArchitecture :: StructGraph -> String
> toArchitecture g =  "\n"
>                  ++ "architecture " ++ name g ++ " of " ++ " >>> TODO <<< " ++ " is"
>                  ++ toComponentSpec g
>                  ++ toSourceSpec g 

> toComponentSpec :: StructGraph -> String
> toComponentSpec g =  "\n"
>                   ++ "component " ++ name g 
>                   ++ "\n"
>                   ++     toPortSpec g
>                   ++ "\n"
>                   ++ "end component;"

> toPortSpec :: StructGraph -> String
> toPortSpec g =  "\n"
>              ++ "port(" ++ "\n"
>              ++ inpins  ++ " : in std_logic;" ++ "\n"
>              ++ outpins ++ " : out std_logic;" ++ "\n"
>              ++ ");" ++ "\n"
>    where inpins  = prtPins $ pins "inpin" sinks 
>          outpins = prtPins $ pins "outpin" sources
>          pins :: String -> (StructGraph -> [AnchorPoint]) -> [String]
>          pins s f  = map (\x -> s ++ (show.snd $ x)) $ filter (isNothing.fst) $ f g
>          prtPins x = foldl1 (\x y -> x ++ ", " ++ y) $ x

> toSourceSpec :: StructGraph -> String
> toSourceSpec g =  "\n"
>                ++ "begin" ++ "\n"
>                ++ "   >>> something in between <<<   " ++ "\n"
>                ++ "end" ++ "\n"


> toSimpleList :: StructGraph -> String
> toSimpleList g =  "\n"
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
