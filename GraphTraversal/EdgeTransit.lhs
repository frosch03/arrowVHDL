This module defines an Algebraic Data Type of a transit structure for edges.
It holds a tuple of a list of anchor points with their according name. The 
anchor points define the start and end point of an edge.

> module GraphTraversal.EdgeTransit ()
> where

> import Data.Maybe ( isJust )

> import GraphTraversal.Core


+-----------------------------+
|                             |
|  [([AnchorPoint], String)]  |
|                             |
+-----------------------------+


At first the transit structure is defined through this type definition.

> type NamedEdge = ([AnchorPoint], String)


Note, that the list of NamedEdges is not explicitly declared.


Functions that work over the named edge list type are:

* generateNamedEdges
  A function that takes Structured Graph 
  and produces a list of named edges

* getAllEdgeNames
  A function that takes a list of named edges
  and produces list of names

* getEdgeName
  A function that takes a list of named edges and an anchor point
  and produces a string


Needed:
[ ] AnchorPoint
[ ] Name (pre :: prefix for signals)
[ ] StructGraph (edges)
[ ] Edge (MkEdge, sourceInfo, sinkInfo)
[ ] DataList (elem)
[ ] DataMaybe (isJust)
[ ]

> pre :: String
> pre = "i"


> generateNamedEdges :: StructGraph -> [NamedEdge]
> generateNamedEdges g 
>     = map (\(i, e) -> (sourceInfo e : sinkInfo e : [], pre ++ show i))
>     $ zip [0..] relevantEdges
>     where relevantEdges = filter (\  (MkEdge (ci,_) (co,_))
>                                   -> isJust ci && isJust co)
>                         $ edges g

> getAllEdgeNames :: [NamedEdge] -> [String]
> getAllEdgeNames = map snd

> getEdgeName :: AnchorPoint -> [NamedEdge] -> String
> getEdgeName ap nedgs
>     = head
>     $ map snd
>     $ filter (\(aps, _) -> ap `elem` aps)
>     $ nedgs
