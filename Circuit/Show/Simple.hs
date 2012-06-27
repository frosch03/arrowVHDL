module Circuit.Show.Simple
( showCircuit
, showEdge
)
where

import Circuit.Descriptor

showCircuit :: CircuitDescriptor -> String
showCircuit g =  showCircuit' g
              ++ "\n" ++ "Area: " ++ (show $ space g)
              ++ "\n" ++ "Time: " ++ (show $ cycles g)

showCircuit' :: CircuitDescriptor -> String
showCircuit' g =  "\n"
            ++ (show.nodeId.nodeDesc) g
            ++ "(" ++ (show.label.nodeDesc) g ++ "): "
            ++ (prtInOuts.sinks.nodeDesc) g ++ "] "
            ++ (showEdges.edges) g
            ++ " [" ++ (prtInOuts.sources.nodeDesc) g
            ++ (showNode.nodes) g
         where showNode [] = ""
               showNode n  = concat $ map showCircuit' n
               prtInOuts [] = "_"
               prtInOuts x  = foldl1 (\x y -> x ++ ',':y) $ map show x


showEdge :: Edge -> String
showEdge ed = (prtConnection.sourceInfo) ed ++ "->" ++ (prtConnection.sinkInfo) ed
      where prtConnection (Just cid, pid) = show (cid, pid)
            prtConnection (Nothing,  pid) = "(_," ++ show pid ++ ")"


showEdges :: [Edge] -> String
showEdges = concat . (map showEdge)

