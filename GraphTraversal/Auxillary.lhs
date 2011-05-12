> module GraphTraversal.Auxillary 
>     ( connect
>     , combine
>     , mkPins
>     , flatten
>     , allCompIDs
>     , allNodes
>     )
> where

> import Control.Arrow

> import Data.List (union, groupBy, (\\))

> import GraphTraversal.Core
> import GraphTraversal.Show


The structured graph is the datatype, that represents the state inside our 
traversal Arrow. While we want to make the TraversalArrow an element of 
Category a function to connect two of these graphs together is needed.e

This connect function generates a new graph out of the two input graphs.
A new name is generated from the names of both inputs, the sinks of the 
left graph become the sinks of the new graph and so are the sources of the 
right one. 
The component id's are updated so that every id is still unique. 

> conn :: ((StructGraph -> StructGraph -> ([Edge], (Pins, Pins))), String) 
>      -> StructGraph -> StructGraph -> StructGraph
> conn (rewire, s) sg_f sg_g = MkSG { name    = (name sg_f') ++ s ++ (name sg_g')
>                                   , compID  = 0
>                                   , nodes   = sg_f'': sg_g'' : []
>                                   , edges   = es
>                                   , sinks   = srcs 
>                                   , sources = snks
>                                   }
>     where sg_f'              = alterCompIDs 1                    sg_f
>           sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
>           (es, (srcs, snks)) = rewire sg_f' sg_g'
>           sg_f''             = sg_f' { edges = onlyInnerEdges $ edges sg_f'}
>           sg_g''             = sg_g' { edges = onlyInnerEdges $ edges sg_g' }


> connect :: StructGraph -> StructGraph -> StructGraph
> connect = conn (seqRewire, "_conn_")

> combine :: StructGraph -> StructGraph -> StructGraph
> combine = conn (parRewire, "_comb_")




> allCompIDs :: StructGraph -> [CompID]
> allCompIDs sg 
>     = if (length next_sg == 0) then cid : []
>                                else cid : (concat $ map allCompIDs next_sg)
>     where next_sg = nodes sg
>           cid     = compID sg 


> newCompID :: StructGraph -> CompID
> newCompID sg = nextID compIDs
>     where compIDs = compID sg : (next $ nodes sg)
>           next []        = []
>           next [innerSG] = compID innerSG : next (nodes innerSG)

> mkPins :: Int -> Pins
> mkPins 0 = []
> mkPins n = [0..n-1]

> nextID :: [CompID] -> CompID
> nextID []    = 0
> nextID [cid] = cid + 1
> nextID cids  = nextID [foldl max 0 cids]


> maxCompID :: StructGraph -> CompID
> maxCompID sg = compID sg `max` (foldl max 0 $ map maxCompID (nodes sg))

> alterCompIDs :: Int -> StructGraph -> StructGraph
> alterCompIDs i sg 
>     = sg { compID = compID sg + i
>          , nodes  = map (alterCompIDs i) $ nodes sg
>          , edges  = map (\ (MkEdge (ci,pi) (co,po)) 
>                         -> (MkEdge (maybe ci (Just.(+i)) $ ci ,pi) 
>                                    (maybe co (Just.(+i)) $ co ,po))
>                         ) $ edges sg
>          }


> onlyInnerEdges :: [Edge] -> [Edge]
> onlyInnerEdges es = es'
>     where es' = filter notIO $ es
>           notIO :: Edge -> Bool
>           notIO (MkEdge (Nothing, _) _) = False
>           notIO (MkEdge _ (Nothing, _)) = False
>           notIO _                       = True

> seqRewire :: StructGraph -> StructGraph -> ([Edge], (Pins, Pins))
> seqRewire sg_l sg_r
>     = (src_edges ++ edgs ++ snk_edges, (super_srcs, super_snks))
>     where (edgs, (srcs_l', snks_r')) =  wire (Just $ compID sg_l) (Just $ compID sg_r) (sources sg_l) (sinks sg_r)
>           super_srcs                 =  [0..(length.sinks   $ sg_l) + length snks_r' -1]
>           super_snks                 =  [0..(length.sources $ sg_r) + length srcs_l' -1]
>           src_edges                  =  let len_snks_l = (length.sinks $ sg_l)
>                                         in (fst $ wire Nothing (Just $ compID sg_l) super_srcs                   (sinks sg_l))
>                                         ++ (fst $ wire Nothing (Just $ compID sg_r) (drop len_snks_l super_srcs) (sinks sg_r))
>           snk_edges                  =  let len_srcs_r = (length.sources $ sg_r)
>                                         in (fst $ wire (Just $ compID sg_r) Nothing (sources sg_r) (super_snks))
>                                         ++ (fst $ wire (Just $ compID sg_l) Nothing (sources sg_l) (drop len_srcs_r super_snks))

> parRewire :: StructGraph -> StructGraph -> ([Edge], (Pins, Pins))
> parRewire sg_u sg_d
>     = (src_edges ++ snk_edges, (super_srcs, super_snks))
>     where super_srcs = [0..(length $ (sources sg_u) ++ (sources sg_d)) -1]
>           super_snks = [0..(length $ (sinks sg_u)   ++ (sinks sg_d))   -1]
>           src_edges  = let len_snks_u = (length.sinks $ sg_u) 
>                        in (fst $ wire Nothing (Just $ compID sg_u) super_srcs                   (sinks sg_u))
>                        ++ (fst $ wire Nothing (Just $ compID sg_d) (drop len_snks_u super_srcs) (sinks sg_d))
>           snk_edges  = let len_srcs_d = (length.sources $ sg_d)
>                        in (fst $ wire (Just $ compID sg_u) Nothing (sources sg_u) super_snks)
>                        ++ (fst $ wire (Just $ compID sg_d) Nothing (sources sg_d) (drop len_srcs_d super_snks))


> wire :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
> wire cid_l cid_r pins_l pins_r 
>     = (edges, (drop cnt pins_l, drop cnt pins_r))
>     where points_l = map ((,) (cid_l)) pins_l
>           points_r = map ((,) (cid_r)) pins_r
>           edges    = map (uncurry MkEdge) $ zip points_l points_r
>           cnt      = length edges


> fromOrToComps :: Edge -> [CompID] -> Bool
> fromOrToComps e cs = foldl (&&) True $ map (fromOrToComp e) cs

> fromOrToComp :: Edge -> CompID -> Bool
> fromOrToComp edg@(MkEdge (Just fromID,_) _) cid = fromComp edg cid
> fromOrToComp edg@(MkEdge _ (Just fromID,_)) cid = toComp   edg cid

> fromComp, toComp :: Edge -> CompID -> Bool
> fromComp (MkEdge (Just fromID,_) _)        cid = cid == fromID
> toComp   (MkEdge _          (Just toID,_)) cid = cid == toID
> toComp edg cid = error $ show $ edg

> fromComps, toComps :: Edge -> [CompID] -> Bool
> fromComps e cs = foldl (&&) True $ map (fromComp e) cs
> toComps   e cs = foldl (&&) True $ map (toComp e)   cs


> mergeEdge :: Edge -> Edge -> Edge
> mergeEdge (MkEdge from _) (MkEdge _ to) = MkEdge from to

> mergeEdge2 :: [Edge] -> Edge
> mergeEdge2 es |  length es > 2  || length es < 2    
>     = error ("How to merge other than 2 edges?" ++ (concat $ map show es))
> mergeEdge2 [(MkEdge from1 to1), (MkEdge from2 to2)]  
>     = if from1 /= to2 
>           then MkEdge from1 to2
>           else MkEdge from2 to1

> samePin :: Edge -> Edge -> Bool
> samePin (MkEdge (_, ip1) (_, op1))
>         (MkEdge (_, ip2) (_, op2))
>     =  op1 == ip2
>     || op2 == ip1


> flatten = error $ show "blub"

> allNodes :: StructGraph -> [StructGraph]
> allNodes g = f g : (emptySubNodes ++ (concat $ map allNodes subNodes))
>     where subNodes      = nodes g
>           emptySubNodes = map f subNodes
>           f n           = n { nodes = [] }



 allEdges :: StructGraph -> [Edges]
 allEdges g = x ++ (concat $ map allEdges subNodes)
     where subNodes      = nodes g 
           



> drop_first  :: (Arrow a) => a (b, b') b'
> drop_first  =  arr (\(x, y) -> y)

> drop_second :: (Arrow a) => a (b, b') b
> drop_second =  arr (\(x, y) -> x)




> emptyGraph :: StructGraph
> emptyGraph = MkSG { name    = ""
>                   , compID  = 0
>                   , nodes   = []
>                   , edges   = []
>                   , sinks   = []
>                   , sources = []
>                   }

 


 unifyPinIDs :: ([AnchorPoint], PinID) -> [AnchorPoint]
 unifyPinIDs (aps, pid) = map (\(x, y) -> (x, y + pid)) aps


 unifyCompID :: StructGraph -> StructGraph
 unifyCompID sg = sg { compID = 0
                     , nodes  = sg'
                     , edges  = es' 
                     }
     where (sg', es', cid') = unifyCompIDs (nodes sg, edges sg, 1)

 unifyCompID' :: (StructGraph, CompID) -> (StructGraph, CompID)
 unifyCompID' (sg, cid) 
     = ( sg { compID = cid
            , nodes  = sub_sg'
            , edges  = es'
            }
       , cid_next
       )
     where (sub_sg', es', cid_next) = unifyCompIDs (nodes sg, edges sg, cid+1)


 unifyCompIDs :: ([StructGraph], [Edge], CompID) -> ([StructGraph], [Edge], CompID)
 unifyCompIDs ([],     es, cid) = ([],         es,   cid)
 unifyCompIDs (sg:sgs, es, cid) = (sg' : sgs', es'', cid'')
     where (sg',  cid')        = unifyCompID' (sg, cid)
           es'                 = fitEdges (compID sg, cid) es
           (sgs', es'', cid'') = unifyCompIDs (sgs, es', cid')





 fitEdges  :: (CompID, CompID) -> [Edge] -> [Edge]
 fitEdges (old_cid, new_cid) = map (fitEdge (old_cid, new_cid))

 fitEdge :: (CompID, CompID) -> Edge -> Edge
 fitEdge (o, n) (MkEdge (Just c, p) s) = MkEdge (if o == c then Just n else Just c, p) s
 fitEdge (o, n) (MkEdge s (Just c, p)) = MkEdge s (if o == c then Just n else Just c, p)
 fitEdge _      _                      = error $ "3: " ++ "this should't happen"
