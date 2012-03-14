> module Grid.Graph
> where

> import Grid.Core


> emptyCircuit :: CircuitDescriptor
> emptyCircuit 
>   = MkDescriptor 
>          { label   = "..."
>          , compID  = 0
>          , nodes   = []
>          , edges   = []
>          , sinks   = []
>          , sources = []
>          }

> arrCircuit 
>   = emptyCircuit { label   = "-ARR-" 
>                , sinks   = [0]
>                , sources = [0]
>                }

> throughCircuit 
>   = emptyCircuit { label   = "(-)"
>                , sinks   = [0]
>                , sources = [0]
>                }

> idCircuit 
>   = emptyCircuit { label   = "-ID-"
>                , sinks   = [0]
>                , sources = [0]
>                }

> leftCircuit 
>   = emptyCircuit { label   = "(L)"
>                , sinks   = []
>                , sources = []
>                }

> rightCircuit 
>   = emptyCircuit { label   = "(R)"
>                , sinks   = []
>                , sources = []
>                }
