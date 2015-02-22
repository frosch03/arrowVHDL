module System.ArrowVHDL.Circuit.Show.DOT
( showCircuit
, showEdge
)
where

import Data.Maybe ( isJust )
import Data.List ( nub
                 , (\\)
                 )

import Prelude hiding ( break ) 

import System.ArrowVHDL.Circuit.Descriptor

import System.ArrowVHDL.Circuit.PinTransit
import System.ArrowVHDL.Circuit.EdgeTransit

import System.ArrowVHDL.Circuit.Show.Tools


-- This function produces the edge-description as it is required by the 
-- dot language... something like this:
--     nodeId3:op0 -nodeId6:ip0

showEdge :: Edge -> String
showEdge (MkEdge (Nothing, pid) (Just snk_cid, snk_pid))
    =  "xSTART" ++ ':':"op" ++ show pid
    ++ " -> "
    ++ "nodeId" ++ show snk_cid ++ ':': "ip" ++ show snk_pid
showEdge (MkEdge (Just src_cid, src_pid) (Nothing, pid)) 
    =  "nodeId" ++ show src_cid ++ ':': "op" ++ show src_pid
    ++ " -> "
    ++ "xEND" ++ ':':"ip" ++ show pid
showEdge e
    =  "nodeId" ++ show src_cid ++ ':': "op" ++ show src_pid
    ++ " -> "
    ++ "nodeId" ++ show snk_cid ++ ':': "ip" ++ show snk_pid
    where (Just src_cid, src_pid) = sourceInfo e
          (Just snk_cid, snk_pid) = sinkInfo e 


showCircuit :: CircuitDescriptor -> String
showCircuit g 
     = concat $ map break
     [ ""
     , "digraph G {"
     , dot_config
     , dot_outer_nodes g
     , dot_components  g
     , dot_connections g 
     , "}"
     ]
     where namedEdges = generateNamedEdges g
           namedComps = generateNamedComps g

dot_config :: String
dot_config
     = concat $ map break
     [ ""
     , "graph ["
     , "    rankdir = \"LR\""
     , "]"
     ] 

dot_outer_nodes :: CircuitDescriptor -> String
dot_outer_nodes g
     = concat $ map break
     [ ""
     , "xSTART ["
     , "    " ++ dot_outer_label "op" (sinks.nodeDesc $ g)
     , "    " ++ "shape = \"record\""
     , "]"
     , ""
     , "xEND ["
     , "    " ++ dot_outer_label "ip" (sources.nodeDesc $ g)
     , "    " ++ "shape = \"record\""
     , "]"
     ]

dot_components :: CircuitDescriptor -> String
dot_components  g
     = concat $ nub $ map f (nodes g)
     where f g' = concat $ map break
                [ ""
                , "nodeId" ++ show (nodeId.nodeDesc $ g') ++ " ["
                , "    " ++ dot_label (sinks.nodeDesc $ g') (map (\x -> if x == '>' then '-' else x) $ label.nodeDesc $ g') (nodeId.nodeDesc $ g') (sources.nodeDesc $ g')
                , "    " ++ "shape = \"record\""
                , "]"
                ] 

dot_outer_label :: String -> Pins -> String
dot_outer_label s ps
     =  "label = \" {{ | " 
     ++ (concat $ map (f s) ps)
     ++ "}}\""
     where f :: String -> Int -> String
           f s x = "<" ++ s ++ show x ++ "> (" ++ show x ++ ") | "

dot_label :: Pins -> String -> CompID -> Pins -> String
dot_label ips nme cid ops
     =  "label = \" {{ | " 
     ++ (concat $ map (f "ip") ips)
     ++ "} | { Name: " ++ nme ++ " | (" ++ show cid ++ ")} | { | "
     ++ (concat $ map (f "op") ops)
     ++ "}}\""
     where f :: String -> Int -> String
           f s x = "<" ++ s ++ show x ++ "> (" ++ show x ++ ") | "


dot_connections :: CircuitDescriptor -> String
dot_connections g 
     = concat $ map (\x -> showEdge x ++ "\n") (edges g)
