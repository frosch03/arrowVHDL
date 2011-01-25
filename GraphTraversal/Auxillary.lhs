> module GraphTraversal.Auxillary 
>     ( connect
>     , combine
>     )
> where

> import Control.Arrow

> import GraphTraversal.Core


The structured graph is the datatype, that represents the state inside our 
traversal Arrow. While we want to make the TraversalArrow an element of 
Category a function to connect two of these graphs together is needed. 

This connect function generates a new graph out of the two input graphs.
A new name is generated from the names of both inputs, the sinks of the 
left graph become the sinks of the new graph and so are the sources of the 
right one. 
The component id's are updated so that every id is still unique. 

> connect :: StructGraph -> StructGraph -> StructGraph
> connect left right = MkSG { name    = (name left') ++ ">>>" ++ (name right')
>                           , compID  = nextID $ (allCompIDs left') ++ (allCompIDs right')
>                           , nodes   = left' : right' : []
>                           , edges   = rewire (sources left', compID left') (sinks right', compID right')
>                           , sinks   = sinks left'
>                           , sources = sources right'
>                           }
>     where [left',right'] = fst.unifyCompIDs $ ([left, right], 0)


> combine :: StructGraph -> StructGraph -> StructGraph
> combine up down = MkSG { name    = (name up') ++ "+++" ++ (name down') 
>                        , compID  = nextID $ (allCompIDs up') ++ (allCompIDs down')
>                        , nodes   = up' : down' : []
>                        , edges   = (edges up') ++ (edges down') 
>                        , sinks   = (sinks up') ++ (sinks down')
>                        , sources = (sources up') ++ (sources down')
>                        }
>     where [up', down'] = fst.unifyCompIDs $ ([up, down], 0)



> allCompIDs :: StructGraph -> [CompID]
> allCompIDs sg 
>     = if (length next_sg == 0) then cid : []
>                                else cid : (concat $ map allCompIDs next_sg)
>     where next_sg = nodes sg
>           cid     = compID sg 


> rewire :: ([SourceEdge], CompID) -> ([SinkEdge], CompID) -> [Edge]
> rewire (srcs, cid_l) (snks, cid_r) 
>     = zipWith (\src snk -> MkEdge (Just $ cid_l, src)
>                                   (Just $ cid_r, snk)) (map snd srcs) (map snd snks)


> unifyCompID :: (StructGraph, CompID) -> (StructGraph, CompID)
> unifyCompID (sg, cid) 
>     = ( sg { compID = cid
>            , nodes  = sub_sg' 
>            }
>       , cid_next
>       )
>     where (sub_sg', cid_next) = unifyCompIDs (nodes sg, (cid+1))


> unifyCompIDs :: ([StructGraph], CompID) -> ([StructGraph], CompID)
> unifyCompIDs ([],     cid) = ([], cid)
> unifyCompIDs (sg:sgs, cid) = (sg' : sgs', cid'')
>     where (sg',  cid')  = unifyCompID  (sg, cid)
>           (sgs', cid'') = unifyCompIDs (sgs, cid')


> newCompID :: StructGraph -> CompID
> newCompID sg = nextID compIDs
>     where compIDs = compID sg : (next $ nodes sg)
>           next []        = []
>           next [innerSG] = compID innerSG : next (nodes innerSG)


> nextID :: [CompID] -> CompID
> nextID []    = 0
> nextID [cid] = cid + 1
> nextID cids  = nextID [foldl max 0 cids]


> drop_first  :: (Arrow a) => a (b, b') b'
> drop_first  =  arr (\(x, y) -> y)

> drop_second :: (Arrow a) => a (b, b') b
> drop_second =  arr (\(x, y) -> x)
