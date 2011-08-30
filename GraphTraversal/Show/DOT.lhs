> module GraphTraversal.Show.DOT
> ( showStructGraph
> , showEdge
> )
> where

> import Data.Maybe ( isJust )
> import Data.List ( nub
>                  , (\\)
>                  )

> import Prelude hiding ( break ) 

> import GraphTraversal.Core

> import GraphTraversal.PinTransit
> import GraphTraversal.EdgeTransit


This function produces the edge-description as it is required by the 
dot language... something like this:
    compID3:op0 -> compID6:ip0

> showEdge :: Edge -> String
> showEdge (MkEdge (Nothing, pid) (Just snk_cid, snk_pid))
>     =  "xSTART" ++ ':':"op" ++ show pid
>     ++ " -> "
>     ++ "compID" ++ show snk_cid ++ ':': "ip" ++ show snk_pid
> showEdge (MkEdge (Just src_cid, src_pid) (Nothing, pid)) 
>     =  "compID" ++ show src_cid ++ ':': "op" ++ show src_pid
>     ++ " -> "
>     ++ "xEND" ++ ':':"ip" ++ show pid
> showEdge e
>     =  "compID" ++ show src_cid ++ ':': "op" ++ show src_pid
>     ++ " -> "
>     ++ "compID" ++ show snk_cid ++ ':': "ip" ++ show snk_pid
>     where (Just src_cid, src_pid) = sourceInfo e
>           (Just snk_cid, snk_pid) = sinkInfo e 


A function is needed, that appends a newline character to the 
end of a string. 

> break :: String -> String
> break =  flip (++) "\n"


> showStructGraph :: StructGraph -> String
> showStructGraph g 
>      = concat $ map break
>      [ ""
>      , "digraph G {"
>      , dot_config
>      , dot_outer_nodes g
>      , dot_components  g
>      , dot_connections g 
>      , "}"
>      ]
>      where namedEdges = generateNamedEdges g
>            namedComps = generateNamedComps g

> dot_config :: String
> dot_config
>      = concat $ map break
>      [ ""
>      , "graph ["
>      , "    rankdir = \"LR\""
>      , "]"
>      ] 

> dot_outer_nodes :: StructGraph -> String
> dot_outer_nodes g
>      = concat $ map break
>      [ ""
>      , "xSTART ["
>      , "    " ++ dot_outer_label "op" (sinks g)
>      , "    " ++ "shape = \"record\""
>      , "]"
>      , ""
>      , "xEND ["
>      , "    " ++ dot_outer_label "ip" (sources g)
>      , "    " ++ "shape = \"record\""
>      , "]"
>      ]

> dot_components :: StructGraph -> String
> dot_components  g
>      = concat $ nub $ map f (nodes g)
>      where f g' = concat $ map break
>                 [ ""
>                 , "compID" ++ show (compID g') ++ " ["
>                 , "    " ++ dot_label (sinks g') (map (\x -> if x == '>' then '-' else x) $ name g') (compID g') (sources g')
>                 , "    " ++ "shape = \"record\""
>                 , "]"
>                 ] 

> dot_outer_label :: String -> Pins -> String
> dot_outer_label s ps
>      =  "label = \" {{ | " 
>      ++ (concat $ map (f s) ps)
>      ++ "}}\""
>      where f :: String -> Int -> String
>            f s x = "<" ++ s ++ show x ++ "> (" ++ show x ++ ") | "

> dot_label :: Pins -> String -> CompID -> Pins -> String
> dot_label ips nme cid ops
>      =  "label = \" {{ | " 
>      ++ (concat $ map (f "ip") ips)
>      ++ "} | { Name: " ++ nme ++ " | (" ++ show cid ++ ")} | { | "
>      ++ (concat $ map (f "op") ops)
>      ++ "}}\""
>      where f :: String -> Int -> String
>            f s x = "<" ++ s ++ show x ++ "> (" ++ show x ++ ") | "


> dot_connections :: StructGraph -> String
> dot_connections g 
>      = concat $ map (\x -> showEdge x ++ "\n") (edges g)
