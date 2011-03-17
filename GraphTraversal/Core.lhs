> module GraphTraversal.Core
> where

> import Data.Maybe ( isNothing
>                   , fromJust
>                   )
> import Prelude hiding (break)


> type PinID  = Int
> type CompID = Int
> type Pins   = [PinID]


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
>          , sinks   :: Pins
>          , sources :: Pins
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
            
> type AnchorPoint = (CompID, PinID)
> type SinkAnchor   = AnchorPoint
> type SourceAnchor = AnchorPoint



To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
>       where prtConnection (cid, pid) = show (cid, pid)

> instance Show (StructGraph) where
>     show = toVHDL
> --  show = toSimpleList


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
>      , vhdl_entity g (map fst namedSuperSinks, map fst namedSuperSources)
>      , vhdl_components g (namedSubSinks, namedSubSources)
>      , vhdl_signals g namedEdges
>      , vhdl_portmaps g (((namedSubSinks ++ namedSuperSinks), (namedSubSources ++ namedSuperSources)), namedEdges)
>      ]
>      where namedSuperSinks   = namePins sinks   "inpin"    g
>            namedSuperSources = namePins sources "outpin"   g
>            namedEdges        = nameEdges        "internal" g
>            namedSubSinks     = concat $ map (namePins sinks   "in")  $ nodes g
>            namedSubSources   = concat $ map (namePins sources "out") $ nodes g


The VHDL-Header is just some boilerplate-code where library's are imported

> vhdl_header :: String
> vhdl_header 
>      = concat $ map break
>      [ "LIBRARY ieee;"
>      , "USE ieee.std_logic_1164.all;"
>      ]


A VHDL-Entity defines an "interface" to a hardware component. It consists of
a name and of some port-definitions (like what wires go inside and come back out)

> vhdl_entity :: StructGraph -> ([String], [String]) -> String
> vhdl_entity g (snks, srcs)
>      = concat $ map break
>      [ "ENTITY " ++ name g ++ " IS"
>      , "PORT (" 
>      , (seperate_with "\n" $ map (\x -> x ++ " IN  std_logic;") snks)
>      , (seperate_with "\n" $ map (\x -> x ++ " OUT std_logic;") srcs)
>      , ");"
>      , "END " ++ name g ++ ";"
>      ]


The VHDL-Component definitions describe the basic interface to the components
that are used inside this new definition. We therefore pick the components 
of which these new component consists. We call this components the level 1 
components, because we descent only one step down in the graph. 

> vhdl_components :: StructGraph -> ([(String, (CompID, PinID))], [(String, (CompID, PinID))]) -> String
> vhdl_components g  (namedSnks, namedSrcs)
>      = concat $ map f (nodes g)
>     where f g' = concat $ map break
>                [ ""
>                , "COMPONENT " ++ name g' ++ "Comp"
>                , "PORT ("
>                , (seperate_with "\n" $ map (\x -> x ++ " IN  std_logic;") $ map fst compSnks)
>                , (seperate_with "\n" $ map (\x -> x ++ " OUT std_logic;") $ map fst compSrcs)
>                , ");"
>                ] 
>               where compSnks = filter (isAtComp $ compID g') namedSnks
>                     compSrcs = filter (isAtComp $ compID g') namedSrcs

> isAtComp :: CompID -> (String, (CompID, PinID)) -> Bool
> isAtComp cid (_, (cid', _)) 
>     = cid == cid'

> isFromOrToComp :: CompID -> (String, Edge) -> Bool
> isFromOrToComp cid (_, (MkEdge from to))
>     =  isAtComp cid ("", from)
>     || isAtComp cid ("", to)



The VHDL-Signals is the list of inner wires, that are used inside the new component.

> vhdl_signals :: StructGraph -> [(String, Edge)] -> String
> vhdl_signals _ [] = ""
> vhdl_signals g namedEdges
>      = "SIGNAL " ++ seperate_with ", " signals ++ ": std_logic" 
>      where signals = map fst namedEdges




> type NamedPins = [(String, (CompID, PinID))]
> type NamedIOs  = (NamedPins, NamedPins)
> type NamedSigs = [(String, Edge)]

> vhdl_portmaps :: StructGraph -> (NamedIOs, NamedSigs) ->  String
> vhdl_portmaps g names@((namedSnks, namedSrcs), namedSigs)
>      = concat $ map break
>      [ "BEGIN"
>      , concat $ map (flip vhdl_portmap names) $ nodes g 
>      , "END"
>      ]
>      where nodes_level1 = nodes g

> vhdl_portmap :: StructGraph -> (NamedIOs, NamedSigs) -> String
> vhdl_portmap g names@((namedSnks, namedSrcs), namedSigs)
>      = concat $ map break
>      [ (name g) ++ "Inst: " ++ (name g) ++ "Comp"
>      , "PORT MAP ("
>      ++ (seperate_with ", " $ (map (\(_, (x, y)) -> x ++ " <= " ++ y)) $ snk_sig_combi ++ src_sig_combi)
>      ++ ");"
>      ]
>      where relevantSnks  = filter (isAtComp       $ compID g) namedSnks
>            relevantSrcs  = filter (isAtComp       $ compID g) namedSrcs
>            relevantSigs  = filter (isFromOrToComp $ compID g) namedSigs
>            compIOs       = map (\x -> (compID g, x)) $ (sinks g ++ sources g)
>            snk_sig_combi = concat $ [[ (a_snk, (s_snk, s_sig)) 
>                              | (s_snk, a_snk)                  <- relevantSnks, a_snk == a2_sig]
>                              | (s_sig, (MkEdge a1_sig a2_sig)) <- relevantSigs]
>            src_sig_combi = concat $ [[ (a_src, (s_src, s_sig)) 
>                              | (s_src, a_src)                  <- relevantSrcs, a_src == a1_sig]
>                              | (s_sig, (MkEdge a1_sig a2_sig)) <- relevantSigs]




With the VHDL-Component-Ports function we generate a nice string with all the
in and output pins, seperated by a comma and a blank. 

> vhdl_component_ports :: StructGraph -> String
> vhdl_component_ports g 
>      = concat $ map break
>      [ name_pins "in" (sinks g)    ++ " : in  std_logic;" 
>      , name_pins "out" (sources g) ++ " : out std_logic;"
>      ]


With the VHDL-Port-Definition function we generate a nice string with all the
in and output pins, seperated by a comma and a blank. 

> vhdl_port_definition :: StructGraph -> String
> vhdl_port_definition g 
>      = concat $ map break
>      [ name_pins "inpin" (sinks g)    ++ " : in  std_logic;"
>      , name_pins "outpin" (sources g) ++ " : out std_logic;"
>      ]

or:

> vhdl_port :: StructGraph -> (String, String) -> String
> vhdl_port g (snk_name, src_name) 
>      = concat $ map break
>      [ name_pins snk_name (sinks g)   ++ " : in  std_logic;"
>      , name_pins src_name (sources g) ++ " : out std_logic;"
>      ]


The Name-Anchors function takes a string and a list of anchor points. The string is the prepended infront of every anchor points number. All the strings are then concated and 
seperated with a comma and a blank. 

> name_pins :: String -> Pins -> String
> name_pins name pins 
>     = seperate_with ", " 
>     $ map (\x -> name ++ (show x)) pins



The namePins function takes a function that extracts a list of PinIDs out of an StructGraph.
(This could be the sinks or the sources functions) 
It also takes a StructGraph (suprise :)) and a String, that is prepended to the actual PinName.
This functions returns a list, where every element is a tuple of the actual named pin (a string)
and a part, that identifies the name.

> namePins :: (StructGraph -> Pins) -> String -> StructGraph -> [(String, (CompID, PinID))]
> namePins f pre g
>     = map (\x -> (pre ++ (show x), (compID g, x))) $ f g


The nameEdges function is pretty similar to the namePins function with some minor differences. 
First of all, you don't need a function that extracts the edges of a StructGraph. There is 
only one field in the StructGraph that holds the edges. 
And also the return-type is a bit simpler, becaus an edge identifies itself, so there is no need
to do this once more.

> nameEdges :: String -> StructGraph -> [(String, Edge)]
> nameEdges pre g
>     = map (\(num, edge) -> (pre ++ (show num), edge)) $ zip [0..] (edges g)





> seperate_with :: String -> [String] -> String
> seperate_with sep []     = ""
> seperate_with sep (x:[]) = x
> seperate_with sep xs     = foldl1 (\x y -> x ++ sep ++ y) xs





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
>          pins :: String -> (StructGraph -> Pins) -> [String]
>          pins s f  = map (\x -> s ++ (show x)) $ f g
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
>                prtInOuts x  = foldl1 (\x y -> x ++ ',':y) $ map show x
