module Circuit.Show.VHDL
( showCircuit
, showEdge
)
where

import Data.Maybe ( isJust )
import Data.List ( nub
                 , (\\)
                 )

import Prelude hiding ( break ) 

import Circuit.Descriptor

import Circuit.PinTransit
import Circuit.EdgeTransit
import Circuit.Tests

import Circuit.Show.Tools


-- The showEdge function is only needed for the Show class. 
-- While VHDL-Code is generated, of this function is made no use ... 

showEdge :: Edge -> String
showEdge ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
      where prtConnection (Just cid, pid) = show (cid, pid)
            prtConnection (Nothing,  pid) = "(_," ++ show pid ++ ")"


-- In a VHDL-Sorce file, there are two main sections, that we need to specify 
-- in order to get working VHDL-Source.
-- 
-- The function that starts the show-process is the toVHDL-function. Here we 
-- define the basic structure of a VHDL-SourceCode with an header, the 
-- entity-definition as well as the component-definition. 
-- TODO: Add the signal-definition and the port-map-definitions

showCircuit :: CircuitDescriptor -> String
showCircuit g 
     = concat $ map break
     [ ""
     , vhdl_header
     , vhdl_entity       g namedComps
     , vhdl_architecture g 
     , vhdl_components   g namedComps
     , vhdl_signals      g namedEdges
     , vhdl_portmaps     g namedComps namedEdges
     ]
     where namedEdges = generateNamedEdges g
           namedComps = generateNamedComps g

nameEdges :: String -> CircuitDescriptor -> [([Anchor], String)]
nameEdges pre g 
    = map (\(i, e) -> (sourceInfo e : sinkInfo e : [], pre ++ show i)) $ zip [0..] relevantEdges
    where relevantEdges = filter (\(MkEdge (ci,_) (co,_)) -> isJust ci && isJust co) $ edges g 

nameGraphPins :: CircuitDescriptor -> [(CompID, ([(PinID, String)], [(PinID, String)]))]
nameGraphPins g = nameSuperPins g : (map nameSubPins $ nodes g)

nameSuperPins :: CircuitDescriptor -> (CompID, ([(PinID, String)], [(PinID, String)]))
nameSuperPins g = (nodeId.nodeDesc $ g, (namedSinks, namedSources))
    where namedSinks   = namePins' (sinks.nodeDesc)   nameExI g 
          namedSources = namePins' (sources.nodeDesc) nameExO g

nameSubPins :: CircuitDescriptor -> (CompID, ([(PinID, String)], [(PinID, String)]))
nameSubPins g = (nodeId.nodeDesc $ g, (namedSinks, namedSources))
    where namedSinks   = namePins' (sinks.nodeDesc)   nameInI g 
          namedSources = namePins' (sources.nodeDesc) nameInO g

namePins' :: (CircuitDescriptor -> Pins) -> String -> CircuitDescriptor -> [(PinID, String)]
namePins' f pre g = map (\x -> (x, pre ++ show x)) $ f g 

-- The VHDL-Header is just some boilerplate-code where library's are imported

vhdl_header :: String
vhdl_header 
     = concat $ map break
     [ "LIBRARY ieee;"
     , "USE ieee.std_logic_1164.all;"
     ]

-- A VHDL-Entity defines an "interface" to a hardware component. It consists of
-- a name and of some port-definitions (like what wires go inside and come back out)

vhdl_entity :: CircuitDescriptor -> [NamedComp] -> String
vhdl_entity g namedComps
     = concat $ map break
     [ "ENTITY " ++ (label.nodeDesc) g ++ " IS"
     , "PORT (" 
     , (sepBy "\n" $ map (\x -> x ++ " : IN  std_logic;") $ snks)
     , (sepBy "\n" $ map (\x -> x ++ " : OUT std_logic ") $ srcs)
     , ");"
     , "END " ++ (label.nodeDesc) g ++ ";"
     ]
     where snks = getInPinNames  namedComps (nodeId.nodeDesc $ g)
           srcs = getOutPinNames namedComps (nodeId.nodeDesc $ g)

vhdl_architecture :: CircuitDescriptor -> String
vhdl_architecture g 
    = "ARCHITECTURE " ++ (label.nodeDesc $ g) ++ "Struct OF " ++ (label.nodeDesc $ g) ++ " IS"


-- The VHDL-Component definitions describe the basic interface to the components
-- that are used inside this new definition. We therefore pick the components 
-- of which these new component consists. We call this components the level 1 
-- components, because we descent only one step down in the graph. 

vhdl_components :: CircuitDescriptor -> [NamedComp] -> String
vhdl_components g namedComps
     = concat $ nub $ map f (nodes g)
    where f g' = concat $ map break
               [ ""
               , "COMPONENT " ++ (label.nodeDesc $ g') ++ "Comp"
               , "PORT ("
               , (sepBy "\n" $ map (\x -> x ++ " : IN  std_logic;") $ snks)
               , (sepBy "\n" $ map (\x -> x ++ " : OUT std_logic ") $ srcs)
               , ");"
               , "END COMPONENT " ++ (label.nodeDesc $ g') ++ "Comp;"
               ] 
               where snks = getInPinNames  namedComps (nodeId.nodeDesc $ g')
                     srcs = getOutPinNames namedComps (nodeId.nodeDesc $ g')


-- The VHDL-Signals is the list of inner wires, that are used inside the new component.

vhdl_signals :: CircuitDescriptor -> [([Anchor], String)] -> String
vhdl_signals _ [] = ""
vhdl_signals g namedEdges
     = "SIGNAL " ++ sepBy ", " signals ++ ": std_logic;" 
     where signals = map snd namedEdges


vhdl_portmaps :: CircuitDescriptor -> [NamedComp] -> [([Anchor], String)] -> String
vhdl_portmaps g namedComps namedEdges
     = concat $ map break
     [ "BEGIN"
     , concat $ map (vhdl_portmap g namedComps namedEdges) $ nodes g
     , "END;"
     ]

vhdl_portmap :: CircuitDescriptor -> [NamedComp] -> [([Anchor], String)] -> CircuitDescriptor -> String
vhdl_portmap superG namedComps namedEdges' g
     = concat $ map break
     [ (label.nodeDesc $ g) ++ "Inst" ++ (show.nodeId.nodeDesc $ g) ++ ": " ++ (label.nodeDesc $ g) ++ "Comp"
     , "PORT MAP ("
     ++ (sepBy ", " $ filter ((>0).length) [incoming, signaling, outgoing])
     ++ ");"
     ]
     where relevantEdges = filter (isFromOrToComp $ nodeId.nodeDesc $ g) $ edges superG
           edge2inside   = filter (isFromOuter)   $ relevantEdges
           edge2outside  = filter (isToOuter)     $ relevantEdges
           pin2signal    = relevantEdges \\ (edge2outside ++ edge2inside)
           incoming      = sepBy ", " $ map (genPortMap namedComps namedEdges' (nodeId.nodeDesc $ g)) $ edge2inside
           outgoing      = sepBy ", " $ map (genPortMap namedComps namedEdges' (nodeId.nodeDesc $ g)) $ edge2outside
           signaling     = sepBy ", " $ map (genPortMap namedComps namedEdges' (nodeId.nodeDesc $ g)) $ pin2signal


genPortMap :: [NamedComp] -> [NamedEdge] -> CompID -> Edge -> String

-- From the inner component to the outside
--  : PORT MAP (a0 =out0, a1 => out1);
--               +--------+
--               |  pi = [0] -
--               |ci = 0  | 
--               |  pi = [1] -
--               +--------+

genPortMap namedComps _ _ (MkEdge (Just ci, pi) (Nothing, po))
    = pinName ++ " => " ++ outName
    where pinName  = getOutPinName namedComps ci       pi
          outName  = getOutPinName namedComps superCid po
          superCid = fst . head $ namedComps


-- From the outside to the inner component
--  : PORT MAP (e0 =in0, e1 => in1);
--                 +--------+
--             -[0] = po  |
--                 |co = 0  |
--             -[1] = po  |
--                 +--------+

genPortMap namedComps _ _ (MkEdge (Nothing, pi) (Just co, po))
    = pinName ++ " => " ++ incName
    where pinName  = getInPinName namedComps co       po
          incName  = getInPinName namedComps superCid pi
          superCid = fst . head $ namedComps


-- From the inner component to an inner signal 
--  : PORT MAP (a0 =i0, a0 => i1);
--   +--------+                        +--------+
--   |  pi = [0] - -----i0------ -> [0] = po  |
--   |ci = 0  |                        |co = 1  |  
--   |  pi = [1] - ------i1----- -> [1] = po  |
--   +--------+                        +--------+

genPortMap namedComps namedEdges ownID (MkEdge ie@(Just ci, pi) oe@(Just co, po))
 | ownID == ci = iPinName ++ " => " ++ iSigName
 | ownID == co = oPinName ++ " => " ++ oSigName
     where iPinName = getOutPinName  namedComps ci pi
           oPinName = getInPinName namedComps co po
           iSigName = getEdgeName namedEdges ie 
           oSigName = getEdgeName namedEdges oe




-- In the last genPortMap function there are some irregularities
-- 
-- TODO TODO TODO / why is it called iPin when the out-pin is gathered with the (map snd) bevore the concat???      
--       where iPinNames = concat $ map snd $ map snd $ filter (\(cid, _) -cid == ci) $ namedGraphPins
-- 
-- TODO TODO TODO / why is it called oPin when the in-pin is gathered with the (map fst) bevore the concat???      
--             oPinNames = concat $ map fst $ map snd $ filter (\(cid, _) -cid == co) $ namedGraphPins
-- 
--             iPinName  = head $ map snd $ filter (\(pid, _) -pid == pi) $ iPinNames
--             oPinName  = head $ map snd $ filter (\(pid, _) -pid == po) $ oPinNames


-- The namePins function takes a function that extracts a list of PinIDs out of an StructGraph.
-- (This could be the sinks or the sources functions) 
-- It also takes a StructGraph (suprise :)) and a String, that is prepended to the actual PinName.
-- This functions returns a list, where every element is a tuple of the actual named pin (a string)
-- and a part, that identifies the name.

namePins :: (CircuitDescriptor -> Pins) -> String -> CircuitDescriptor -> [(String, Anchor)]
namePins f pre g
    = map (\x -> (pre ++ (show x), (Nothing, x))) $ f g
--  = map (\x -> (pre ++ (show x), (nodeId g, x))) $ f g


-- The nameEdges function is pretty similar to the namePins function with some minor differences. 
-- First of all, you don't need a function that extracts the edges of a StructGraph. There is 
-- only one field in the StructGraph that holds the edges. 
-- And also the return-type is a bit simpler, becaus an edge identifies itself, so there is no need
-- to do this once more.

-- nameEdges :: String -> CircuitDescriptor -> [(String, Edge)]
-- nameEdges pre g
--     = map (\(num, edge) -> (pre ++ (show num), edge)) $ zip [0..] (edges g)


sepBy :: String -> [String] -> String
sepBy sep []     = ""
sepBy sep (x:[]) = x
sepBy sep xs     = foldl1 (\x y -> x ++ sep ++ y) xs
