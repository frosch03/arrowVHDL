> module GraphTraversal.Graph
> where

> import GraphTraversal.Core


> emptyGraph :: Circuit
> emptyGraph 
>   = MkSG { label   = "..."
>          , compID  = 0
>          , nodes   = []
>          , edges   = []
>          , sinks   = []
>          , sources = []
>          }

> arrGraph 
>   = emptyGraph { label   = "-ARR-" 
>                , sinks   = [0]
>                , sources = [0]
>                }

> throughGraph 
>   = emptyGraph { label   = "(-)"
>                , sinks   = [0]
>                , sources = [0]
>                }

> idGraph 
>   = emptyGraph { label   = "-ID-"
>                , sinks   = [0]
>                , sources = [0]
>                }

> leftGraph 
>   = emptyGraph { label   = "(L)"
>                , sinks   = []
>                , sources = []
>                }

> rightGraph 
>   = emptyGraph { label   = "(R)"
>                , sinks   = []
>                , sources = []
>                }
