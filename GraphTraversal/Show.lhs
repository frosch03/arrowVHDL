> module GraphTraversal.Show
> where

> import GraphTraversal.Core
> import GraphTraversal.Show.Simple
> -- import GraphTraversal.Show.VHDL


To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show = showEdge


> instance Show (StructGraph) where
>     show = showStructGraph
