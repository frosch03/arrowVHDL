> module GraphTraversal.Show
> where

Btw, for what ever reason, one can't just reload, because this leads to a 
missing object-file error. Exit the ghci and restart it, will do the job.

> import GraphTraversal.Core
> -- import GraphTraversal.Show.Simple
> import GraphTraversal.Show.VHDL
> -- import GraphTraversal.Show.DOT
> import qualified GraphTraversal.Show.Simple as Simple
> import qualified GraphTraversal.Show.DOT as DOT
> import qualified GraphTraversal.Show.VHDL as VHDL


To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show = showEdge


> instance Show (StructGraph) where
>     show = showStructGraph
