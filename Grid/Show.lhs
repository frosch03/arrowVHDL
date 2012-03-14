> module Grid.Show
> where

Btw, for what ever reason, one can't just reload, because this leads to a 
missing object-file error. Exit the ghci and restart it, will do the job.

> import Grid.Core
> -- import Grid.Show.Simple
> import Grid.Show.VHDL
> -- import Grid.Show.DOT
> import qualified Grid.Show.Simple as Simple
> import qualified Grid.Show.DOT as DOT
> import qualified Grid.Show.VHDL as VHDL


To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
therefore the Edge datatypes also needs to be an instance of Show. 

> instance Show (Edge) where
>   show = showEdge


> instance Show (CircuitDescriptor) where
>     show = showCircuit
