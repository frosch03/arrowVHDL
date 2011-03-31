> module GraphTraversal.Core
> where

> import Data.Maybe ( isNothing
>                   , isJust
>                   , fromJust
>                   )
> import Data.List ( nub 
>                  , (\\)
>                  )
> import Prelude hiding (break)


First of all, lets name some basic types that are used through the whole
source...
There are the pins of a hardware component, and they are identified by an 
integer. The same holds for the identification of a whole component, which 
is also done by an integer. And than there is also a name for the list of
pins that a component holds (either the sink pins or the source pins).

> type PinID  = Int
> type CompID = Int
> type Pins   = [PinID]


An edge is like a wire between two pins on different components. So it is
identified by the component id and the pin id. This tupel is called an
AnchorPoint, and for documentation reasons there are two different versions,
the SinkAnchor and the SourceAnchor.

> type AnchorPoint  = (Maybe CompID, PinID)
> type SinkAnchor   = AnchorPoint
> type SourceAnchor = AnchorPoint


To translate the graph structure into a VHDL-sourcecode the anchors and the
edges are named and stored in lookup tables. There are two of them, one for 
the named pins and one for the named edges. For documentation there is also
a input output tuple, that holds the lookup table for the sinks and the 
sources.

> type NamedPins = [(String, AnchorPoint)]
> type NamedSigs = [(String, Edge)]
> type NamedSnks = NamedPins
> type NamedSrcs = NamedPins
> type NamedIOs  = (NamedSnks, NamedSrcs)

> nameSig = "i"
> nameExI = "in"
> nameExO = "out"
> nameInI = "e"
> nameInO = "a"


The structured graph is the fundamental datatype. It holds information
about its nodes and is identified by an unique id. Additionally the in- 
and out pins are listed as well as connections (edges) between the nodes.
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int

Also a name is needed later on to identify the components.
  Name          := String

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

   deriving (Show)

Remember, a Sink is something that takes something  (INPUT)
and a Source is something that produces something (OUTPUT)

So the next datatype to be defined is an edge. The edge knows 
the pin/component it comes from, as well as the pin/component
it goes to.

> data Edge
>   = MkEdge { sourceInfo :: SourceAnchor
>            , sinkInfo   :: SinkAnchor
>            }
>   deriving (Eq)

   deriving (Eq, Show)

A connection is defined by the tuple of componentID and a pinID
There are two special types of edges, those that come from the
outside into the component (called SinkAnchor). And those that go
from the component to the outside (called SourceAnchor). 
            
To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
>       where prtConnection (Just cid, pid) = show (cid, pid)
>             prtConnection (Nothing,  pid) = "(_," ++ show pid ++ ")"

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
>      , vhdl_entity g namedGraphPins
>      , "ARCHITECTURE"
>      , vhdl_components g namedGraphPins
>      , vhdl_signals g namedEdges'
>      , vhdl_portmaps g namedGraphPins namedEdges'
>      ]
>      where namedSuperSinks   = namePins sinks   nameExI g
>            namedSuperSources = namePins sources nameExO g
>            namedEdges        = nameEdges        nameSig g
>            namedSubSinks     = concat $ map (namePins sinks   nameInI) $ nodes g
>            namedSubSources   = concat $ map (namePins sources nameInO) $ nodes g
>            namedEdges'       = nameEdges' nameSig g
>            namedGraphPins    = nameGraphPins g

> nameEdges' :: String -> StructGraph -> [([AnchorPoint], String)]
> nameEdges' pre g 
>     = map (\(i, e) -> (sourceInfo e : sinkInfo e : [], pre ++ show i)) $ zip [0..] relevantEdges
>     where relevantEdges = filter (\(MkEdge (ci,_) (co,_)) -> isJust ci && isJust co) $ edges g 

> nameGraphPins :: StructGraph -> [(CompID, ([(PinID, String)], [(PinID, String)]))]
> nameGraphPins g = nameSuperPins g : (map nameSubPins $ nodes g)

> nameSuperPins :: StructGraph -> (CompID, ([(PinID, String)], [(PinID, String)]))
> nameSuperPins g = (compID g, (namedSinks, namedSources))
>     where namedSinks   = namePins' sinks   nameExI g 
>           namedSources = namePins' sources nameExO g

> nameSubPins :: StructGraph -> (CompID, ([(PinID, String)], [(PinID, String)]))
> nameSubPins g = (compID g, (namedSinks, namedSources))
>     where namedSinks   = namePins' sinks   nameInI g 
>           namedSources = namePins' sources nameInO g

> namePins' :: (StructGraph -> Pins) -> String -> StructGraph -> [(PinID, String)]
> namePins' f pre g = map (\x -> (x, pre ++ show x)) $ f g 

The VHDL-Header is just some boilerplate-code where library's are imported

> vhdl_header :: String
> vhdl_header 
>      = concat $ map break
>      [ "LIBRARY ieee;"
>      , "USE ieee.std_logic_1164.all;"
>      ]

A VHDL-Entity defines an "interface" to a hardware component. It consists of
a name and of some port-definitions (like what wires go inside and come back out)

> vhdl_entity :: StructGraph -> [(CompID, ([(PinID, String)], [(PinID, String)]))] -> String
> vhdl_entity g namedGraph
>      = concat $ map break
>      [ "ENTITY " ++ name g ++ " IS"
>      , "PORT (" 
>      , (seperate_with "\n" $ map (\x -> x ++ " IN  std_logic;") $ map snd snks)
>      , (seperate_with "\n" $ map (\x -> x ++ " OUT std_logic;") $ map snd srcs)
>      , ");"
>      , "END " ++ name g ++ ";"
>      ]
>      where (_, (snks, srcs)) = head $ filter ((== compID g).fst) namedGraph 


The VHDL-Component definitions describe the basic interface to the components
that are used inside this new definition. We therefore pick the components 
of which these new component consists. We call this components the level 1 
components, because we descent only one step down in the graph. 

> vhdl_components :: StructGraph -> [(CompID, ([(PinID, String)], [(PinID, String)]))] -> String
> vhdl_components g namedGraph
>      = concat $ map f (nodes g)
>     where f g' = concat $ map break
>                [ "COMPONENT " ++ name g' ++ "Comp"
>                , "PORT ("
>                , (seperate_with "\n" $ map (\x -> x ++ " IN  std_logic;") $ map snd snks)
>                , (seperate_with "\n" $ map (\x -> x ++ " OUT std_logic;") $ map snd srcs)
>                , ");"
>                ] 
>               where (_, (snks, srcs)) = head $ filter ((== compID g').fst) namedGraph


The VHDL-Signals is the list of inner wires, that are used inside the new component.

> vhdl_signals :: StructGraph -> [([AnchorPoint], String)] -> String
> vhdl_signals _ [] = ""
> vhdl_signals g namedEdges
>      = "SIGNAL " ++ seperate_with ", " signals ++ ": std_logic" 
>      where signals = map snd namedEdges


> vhdl_portmaps :: StructGraph -> [(CompID, ([(PinID, String)], [(PinID, String)]))] -> [([AnchorPoint], String)] -> String
> vhdl_portmaps g namedGraphPins namedEdges' 
>      = concat $ map break
>      [ "BEGIN"
>      , concat $ map (vhdl_portmap g namedGraphPins namedEdges') $ nodes g
>      , "END"
>      ]

> vhdl_portmap :: StructGraph -> [(CompID, ([(PinID, String)], [(PinID, String)]))] -> [([AnchorPoint], String)] -> StructGraph -> String
> vhdl_portmap superG namedGraphPins namedEdges' g
>      = concat $ map break
>      [ (name g) ++ "Inst: " ++ (name g) ++ "Comp"
>      , "PORT MAP ("
>      ++ (seperate_with ", " $ filter ((>0).length) [incoming, signaling, outgoing])
>      ++ ");"
>      ]
>      where relevantEdges = filter (isFromOrToComp' $ compID g) $ edges superG
>            edge2inside   = filter (fromOutside') $ relevantEdges
>            edge2outside  = filter (toOutside')   $ relevantEdges
>            pin2signal    = relevantEdges \\ (edge2outside ++ edge2inside)
>            incoming      = seperate_with ", " $ map (edge2PortMap' namedGraphPins namedEdges' (compID g)) $ edge2inside
>            outgoing      = seperate_with ", " $ map (edge2PortMap' namedGraphPins namedEdges' (compID g)) $ edge2outside
>            signaling     = seperate_with ", " $ map (edge2PortMap' namedGraphPins namedEdges' (compID g)) $ pin2signal


> edge2PortMap' :: [(CompID, ([(PinID, String)], [(PinID, String)]))] -> [([AnchorPoint], String)] -> CompID -> Edge -> String

From the inner component to the outside
 : PORT MAP (a0 => out0, a1 => out1);
              +--------+
              |  pi = [0] -> 
              |ci = 0  | 
              |  pi = [1] -> 
              +--------+

> edge2PortMap' namedGraphPins _ _ (MkEdge (Just ci, pi) (Nothing, po))
>     = pinName ++ " => " ++ outName
>     where pinNames = concat $ map snd $ map snd $ filter (\(cid, _) -> cid == ci) $ namedGraphPins
>           pinName  = head $ map snd $ filter (\(pid, _) -> pid == pi) $ pinNames
>           outName  = head $ map snd $ filter (\(pid, _) -> pid == po) $ snd $ snd $ head $ namedGraphPins 


From the outside to the inner component
 : PORT MAP (e0 => in0, e1 => in1);
                +--------+
            -> [0] = po  |
                |co = 0  |
            -> [1] = po  |
                +--------+

> edge2PortMap' namedGraphPins _ _ (MkEdge (Nothing, pi) (Just co, po))
>     = pinName ++ " => " ++ incName
>     where pinNames = concat $ map fst $ map snd $ filter (\(cid, _) -> cid == co) $ namedGraphPins
>           pinName  = head $ map snd $ filter (\(pid, _) -> pid == po) $ pinNames
>           incName  = head $ map snd $ filter (\(pid, _) -> pid == pi) $ fst $ snd $ head $ namedGraphPins


From the inner component to an inner signal 
 : PORT MAP (a0 => i0, a0 => i1);
  +--------+                        +--------+
  |  pi = [0] ->  -----i0------ -> [0] = po  |
  |ci = 0  |                        |co = 1  |  
  |  pi = [1] ->  ------i1----- -> [1] = po  |
  +--------+                        +--------+

> edge2PortMap' namedGraphPins namedEdges ownID (MkEdge ie@(Just ci, pi) oe@(Just co, po))
>  | ownID == ci = iPinName ++ " => " ++ iSigName
>  | ownID == co = oPinName ++ " => " ++ oSigName
>      where iPinNames = concat $ map snd $ map snd $ filter (\(cid, _) -> cid == ci) $ namedGraphPins
>            iPinName  = head $ map snd $ filter (\(pid, _) -> pid == pi) $ iPinNames
>            iSigName  = head $ map snd $ filter (\(aps, _) -> ie `elem` aps) $ namedEdges
>            oPinNames = concat $ map fst $ map snd $ filter (\(cid, _) -> cid == co) $ namedGraphPins
>            oPinName  = head $ map snd $ filter (\(pid, _) -> pid == po) $ oPinNames
>            oSigName  = head $ map snd $ filter (\(aps, _) -> oe `elem` aps) $ namedEdges

> isFromOrToComp' :: CompID -> Edge -> Bool
> isFromOrToComp' cid (MkEdge (Nothing, pi) (Just co, po)) = cid == co
> isFromOrToComp' cid (MkEdge (Just ci, pi) (Nothing, po)) =  cid == ci
> isFromOrToComp' cid (MkEdge (Just ci, pi) (Just co, po)) =  cid == co 
>                                                          || cid == ci

> fromOutside' :: Edge -> Bool
> fromOutside' (MkEdge (Nothing, _) _) = True
> fromOutside' otherwise               = False

> toOutside' :: Edge -> Bool
> toOutside' (MkEdge _ (Nothing, _)) = True
> toOutside' otherwise               = False

> isFromComp' :: CompID -> Edge -> Bool 
> isFromComp' cid (MkEdge (Just ci, _) _) = cid == ci
> isFromComp' _   _                       = False

> isToComp' :: CompID -> Edge -> Bool 
> isToComp' cid (MkEdge _ (Just co, _)) = cid == co
> isToComp' _   _                       = False


 vhdl_portmap :: StructGraph -> (NamedIOs, NamedSigs) -> String
 vhdl_portmap g names@((namedSnks, namedSrcs), namedSigs)
      = concat $ map break
      [ (name g) ++ "Inst: " ++ (name g) ++ "Comp"
      , "PORT MAP ("
      ++ (seperate_with ", " $ (map (\(_, (x, y)) -> x ++ " => " ++ y)) $ snk_sig_combi ++ src_sig_combi)
      ++ ");"
      ]
     where f :: Edge -> String
           f (MkEdge (Just ci, pi) (Just co, po)) = 

      where relevantSnks  = filter (isAtComp       $ compID g) namedSnks
            relevantSrcs  = filter (isAtComp       $ compID g) namedSrcs
            relevantSigs  = filter (isFromOrToComp $ compID g) namedSigs
            compIOs       = map (\x -> (compID g, x)) $ (sinks g ++ sources g)
            snk_sig_combi = concat $ [[ (a_snk, (s_snk, s_sig)) 
                              | (s_snk, a_snk)                  <- relevantSnks, a_snk == a2_sig]
                              | (s_sig, (MkEdge a1_sig a2_sig)) <- relevantSigs]
            src_sig_combi = concat $ [[ (a_src, (s_src, s_sig)) 
                              | (s_src, a_src)                  <- relevantSrcs, a_src == a1_sig]
                              | (s_sig, (MkEdge a1_sig a2_sig)) <- relevantSigs]




The namePins function takes a function that extracts a list of PinIDs out of an StructGraph.
(This could be the sinks or the sources functions) 
It also takes a StructGraph (suprise :)) and a String, that is prepended to the actual PinName.
This functions returns a list, where every element is a tuple of the actual named pin (a string)
and a part, that identifies the name.

> namePins :: (StructGraph -> Pins) -> String -> StructGraph -> [(String, AnchorPoint)]
> namePins f pre g
>     = map (\x -> (pre ++ (show x), (Nothing, x))) $ f g
> --  = map (\x -> (pre ++ (show x), (compID g, x))) $ f g


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


> isIOPort :: (String, AnchorPoint) -> Bool
> isIOPort (_, (Nothing, _)) = True
> isIOPort otherwise         = False

> isAtComp :: CompID -> (String, AnchorPoint) -> Bool
> isAtComp cid (_, (Just cid', _)) 
>     = cid == cid'

> isFromOrToComp :: CompID -> (String, Edge) -> Bool
> isFromOrToComp cid (_, (MkEdge from to))
>     =  isAtComp cid ("", from)
>     || isAtComp cid ("", to)






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
