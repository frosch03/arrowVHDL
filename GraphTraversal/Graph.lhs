> module GraphTraversal.Graph
> where

> import GraphTraversal.Core


> emptyGraph :: Circuit
> emptyGraph 
>   = MkSG { name    = "..."
>          , compID  = 0
>          , nodes   = []
>          , edges   = []
>          , sinks   = []
>          , sources = []
>          }

> arrGraph 
>   = emptyGraph { name    = "-ARR-" 
>                , sinks   = [0]
>                , sources = [0]
>                }

> throughGraph 
>   = emptyGraph { name    = "(-)"
>                , sinks   = [0]
>                , sources = [0]
>                }

> idGraph 
>   = emptyGraph { name    = "-ID-"
>                , sinks   = [0]
>                , sources = [0]
>                }

> leftGraph 
>   = emptyGraph { name    = "(L)"
>                , sinks   = []
>                , sources = []
>                }

> rightGraph 
>   = emptyGraph { name    = "(R)"
>                , sinks   = []
>                , sources = []
>                }
