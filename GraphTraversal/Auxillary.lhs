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
>                           , compID  = newCompID
>                           , nodes   = left' : right' : []
>                           , edges   = edgs
>                           , sinks   = srcs 
>                           , sources = snks
>                           }
>     where (edgs, (srcs, snks)) = rewire newCompID left' right'
>           [left',right']       = fst.unifyCompIDs $ ([left, right], 0)
>           newCompID            = nextID $ (allCompIDs left') ++ (allCompIDs right')


> combine :: StructGraph -> StructGraph -> StructGraph
> combine up down = MkSG { name    = (name up') ++ "+++" ++ (name down') 
>                        , compID  = nextID $ (allCompIDs up') ++ (allCompIDs down')
>                        , nodes   = up' : down' : []
>                        , edges   = (edges up') ++ (edges down') 
>                        , sinks   = (sinks up')   ++ (sinksDown)
>                        , sources = (sources up') ++ (sourcesDown)
>                        }
>     where [up', down'] = fst.unifyCompIDs $ ([up, down], 0)
>           sinkCntUp    = length.map snd.sinks   $ up'  
>           sourceCntUp  = length.map snd.sources $ up'  
>           sinksDown    = unifyPinIDs ((sinks   down'), sinkCntUp)
>           sourcesDown  = unifyPinIDs ((sources down'), sourceCntUp)

> unifyPinIDs :: ([AnchorPoint], PinID) -> [AnchorPoint]
> unifyPinIDs (aps, pid) = map (\(x, y) -> (x, y + pid)) aps



> allCompIDs :: StructGraph -> [CompID]
> allCompIDs sg 
>     = if (length next_sg == 0) then cid : []
>                                else cid : (concat $ map allCompIDs next_sg)
>     where next_sg = nodes sg
>           cid     = compID sg 


> rewire :: CompID -> StructGraph -> StructGraph -> ([Edge], (Pins, Pins))
> rewire cid sg_l sg_r
>     = (edgs ++ src_edges ++ snk_edges, (super_srcs, super_snks))
>     where (edgs, (srcs_l', snks_r')) =  wire (compID sg_l) (compID sg_r) (sources sg_l) (sinks sg_r)
>           super_snks                 =  [0..(length (sinks   sg_l) + length snks_r' -1)]
>           super_srcs                 =  [0..(length (sources sg_r) + length srcs_l' -1)]
>           src_edges                  =  (fst $ wire cid (compID sg_l) super_srcs (sinks sg_l))
>                                      ++ (fst $ wire cid (compID sg_r) super_srcs (sinks sg_l))
>           snk_edges                  =  (fst $ wire (compID sg_l) cid (sources sg_l) super_snks)
>                                      ++ (fst $ wire (compID sg_r) cid (sources sg_r) super_snks)

> wire :: CompID -> CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
> wire cid_l cid_r pins_l pins_r 
>     = (edges, (drop cnt pins_l, drop cnt pins_r))
>     where points_l = map ((,) cid_l) pins_l
>           points_r = map ((,) cid_r) pins_r
>           edges    = map (uncurry MkEdge) $ zip points_l points_r
>           cnt      = length edges



While the CompID's are unified it is necessary to also update the edges. The fitedges
functions takes an old and a new component id together with a list of edges that need
to be adopted. Every edge, that comes from or goes to an CompID which is updated, is 
updated also. 

> type OldCID = CompID
> type NewCID = CompID

> fitedges :: (OldCID, NewCID) -> [Edge] -> [Edge]
> fitedges (_    , _    ) []     = []
> fitedges (o_cid, n_cid) (e:es) = MkEdge (replaceCID . sourceInfo $ e) 
>                                         (replaceCID . sinkInfo   $ e) 
>                                : fitedges (o_cid, n_cid) es  
>     where (src_cid, snk_cid) = (fst.sourceInfo $ e, fst.sinkInfo $ e)
>           replaceCID :: AnchorPoint -> AnchorPoint
>           replaceCID ((Nothing),  pid) = (Nothing, pid) 
>           replaceCID ((Just cid), pid) = if cid == o_cid then ((Just n_cid), pid)
>                                                          else ((Just cid), pid) 

> fitedges' :: (OldCID, NewCID) -> [StructGraph] -> [StructGraph]
> fitedges' (_,     _)     []  = []
> fitedges' (o_cid, n_cid) sgs = sgs'
>     where ess  = map (fitedges (o_cid, n_cid) . edges) $ sgs
>           sgs' = zipWith (\sg es -> sg { edges = es }) sgs ess




> unifyCompID :: (StructGraph, CompID) -> (StructGraph, CompID)
> unifyCompID (sg, cid) 
>     = ( sg { compID = cid
>            , nodes  = sub_sg'
>            }
>       , cid_next
>       )
>     where (sub_sg', cid_next) = unifyCompIDs (nodes sg, cid+1)
>           old_cid             = compID sg


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
