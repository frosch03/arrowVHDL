> module GraphTraversal.Auxillary 
> where

> -- import Control.Arrow hiding (Arrow)

> import Data.List (union, groupBy, (\\), isInfixOf, nub)
> import Data.Maybe 
> import Data.Either
> import Control.Monad (msum)

> import GHC.Exts (sortWith)

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
If a connection between the empty graph and another graph is made, that 
connection is only the remaining graph (which is represented by the first 
two lines).

> conn :: ((Circuit -> Circuit -> ([Edge], (Pins, Pins))), String) 
>      -> Circuit -> Circuit -> Circuit
> conn _           sg NoSG   = sg
> conn _           NoSG sg   = sg
> conn (rewire, s) sg_f sg_g = conn' (rewire, s) sg_f sg_g


> conn' :: ((Circuit -> Circuit -> ([Edge], (Pins, Pins))), String) 
>       -> Circuit -> Circuit -> Circuit
> conn' (rewire, s) sg_f sg_g 
>     = MkSG { name    = (name sg_f') ++ s ++ (name sg_g')
>            , compID  = 0
>            , nodes   = sg_f': sg_g' : []
>            , edges   = es
>            , sinks   = srcs 
>            , sources = snks
>            }
>     where sg_f'              = alterCompIDs 1                    sg_f
>           sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
>           (es, (srcs, snks)) = rewire sg_f' sg_g'


> connect :: Circuit -> Circuit -> Circuit
> connect = conn (seqRewire, ">>>")

> combine :: Circuit -> Circuit -> Circuit
> combine = conn (parRewire, "&&&")

> dupCombine :: Circuit -> Circuit -> Circuit
> dupCombine = conn (dupParRewire, ">2>")


> allCompIDs :: Circuit -> [Circuit]
> allCompIDs sg 
>     = if (length next_sg == 0) then sg : []
>                                else sg : (concat $ map allCompIDs next_sg)
>     where next_sg = nodes sg
>           cid     = compID sg 


> newCompID :: Circuit -> CompID
> newCompID sg = nextID compIDs
>     where compIDs = compID sg : (next $ nodes sg)
>           next []        = []
>           next [innerSG] = compID innerSG : next (nodes innerSG)

> mkPins :: Int -> Pins
> mkPins 0 = error $ show "It is not possible to generate a component with 0 pins"
> mkPins n = [0..n-1]

> nextID :: [CompID] -> CompID
> nextID []    = 0
> nextID [cid] = cid + 1
> nextID cids  = nextID [foldl max 0 cids]


> maxCompID :: Circuit -> CompID
> maxCompID sg = compID sg `max` (foldl max 0 $ map maxCompID (nodes sg))

> alterCompIDs :: Int -> Circuit -> Circuit
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

> seqRewire :: Circuit -> Circuit -> ([Edge], (Pins, Pins))
> seqRewire sg_l sg_r
>     = ( fromOuterToL ++ fromOuterToR ++ edgs ++ fromRToOuter ++ fromLToOuter
>       , (super_srcs, super_snks)
>       )
>     where (edgs, (srcs_l', snks_r')) =  wire (Just $ compID sg_l) (Just $ compID sg_r) (sources sg_l) (sinks sg_r)
>           super_srcs                 =  [0..(length.sinks   $ sg_l) + length snks_r' -1]
>           super_snks                 =  [0..(length.sources $ sg_r) + length srcs_l' -1]
>           ( fromOuterToL, (super_srcs', _)) =  wire Nothing (Just $ compID sg_l) super_srcs  (sinks sg_l)
>           ( fromOuterToR, (_          , _)) =  wire Nothing (Just $ compID sg_r) super_srcs' (drop (length fromOuterToL) $ sinks sg_r)
>           ( fromRToOuter, (_, super_snks')) =  wire (Just $ compID sg_r) Nothing (sources sg_r) super_snks
>           ( fromLToOuter, (_, _))           =  wire (Just $ compID sg_l) Nothing (drop (length fromRToOuter) $ sources sg_l) super_snks'

> parRewire :: Circuit -> Circuit -> ([Edge], (Pins, Pins))
> parRewire sg_u sg_d
>     = ( goingIn_edges ++ goingOut_edges
>       , (super_srcs, super_snks)
>       )
>     where super_srcs = [0..(length $ (sinks   sg_u) ++ (sinks   sg_d)) -1]
>           super_snks = [0..(length $ (sources sg_u) ++ (sources sg_d)) -1]
>           goingIn_edges  =  (wire_ Nothing (Just $ compID sg_u)                            (super_srcs) (sinks sg_u))
>                          ++ (wire_ Nothing (Just $ compID sg_d) (drop (length.sinks $ sg_u) super_srcs) (sinks sg_d))
>           goingOut_edges =  (wire_ (Just $ compID sg_u) Nothing (sources sg_u)                              (super_snks))
>                          ++ (wire_ (Just $ compID sg_d) Nothing (sources sg_d) (drop (length.sources $ sg_u) super_snks))

> dupParRewire :: Circuit -> Circuit -> ([Edge], (Pins, Pins))
> dupParRewire sg_u sg_d
>     = ( goingIn_edges ++ goingOut_edges
>       , (super_srcs, super_snks)
>       )
>     where super_srcs = [0..(length.sinks $ sg_u) -1]
>           super_snks = [0..(length $ (sources sg_u) ++ (sources sg_d)) -1]
>           goingIn_edges  =  (wire_ Nothing (Just $ compID sg_u) super_srcs (sinks sg_u))
>                          ++ (wire_ Nothing (Just $ compID sg_d) super_srcs (sinks sg_d))
>           goingOut_edges =  (wire_ (Just $ compID sg_u) Nothing (sources sg_u)                              (super_snks))
>                          ++ (wire_ (Just $ compID sg_d) Nothing (sources sg_d) (drop (length.sources $ sg_u) super_snks))


> wire :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
> wire cid_l cid_r pins_l pins_r 
>     = (edges, (drop cnt pins_l, drop cnt pins_r))
>     where points_l = map ((,) (cid_l)) pins_l
>           points_r = map ((,) (cid_r)) pins_r
>           edges    = map (uncurry MkEdge) $ zip points_l points_r
>           cnt      = length edges

> wire_ :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> [Edge]
> wire_ cid_l cid_r pins_l pins_r = fst $ wire cid_l cid_r pins_l pins_r


> old_flatten :: Circuit -> Circuit 
> old_flatten g = g' { nodes = atomgraphs ++ (concat $ map nodes subgraphs)
>                , edges = (((edges g) \\ delEs) ++ newEs ++ neutralEs)  
>                }
>     where g'         = g { nodes = map old_flatten (nodes g) } 
>           subgraphs  = filter (not.null.nodes) $ nodes g' 
>           atomgraphs = filter (    null.nodes) $ nodes g'
>           allEs      = map (partitionEdges g') subgraphs
>           newEss     = map fst allEs
>           newEs      = concat $ map fst newEss
>           neutralEs  = concat $ map snd newEss
>           delEs      = concat $ map snd allEs


To generate the new edges, the edges are split into edges that
    - come from a sub-graph 
    - go to a sub-graph
    - come from a super-graph
    - go to a super-graph
With their help the graph in between can be resolved and a new edge is generated
that comes from a super-graph and connects with no indirection to the sub-graph.
It is possible to have multiple edges that originate in an exclusiv pin of the
super-graph. These edges are grouped and the according number of edges that point 
to the intermediate pin, are generated with repeat.

> partitionEdges :: Circuit -> Circuit -> (([Edge], [Edge]), [Edge])
> partitionEdges superG subG  = ( ( newIncs ++ newOuts
>                                 , neutrals
>                                 )
>                               , toSubG  ++ fromSubG
>                               )
>     where fromSubG     = filter ((==Just (compID subG)).fst.sourceInfo) $ edges superG
>           toSubG       = filter ((==Just (compID subG)).fst.sinkInfo)   $ edges superG
>
>           fromNothing  = filter (isNothing.fst.sourceInfo)              $ edges subG
>           fromNothing' = groupBy (\x y -> (srcPin x) == (srcPin y)) $ sortWith srcPin fromNothing
> 
>           toNothing    = filter (isNothing.fst.sinkInfo)                $ edges subG
>           toNothing'   = groupBy (\x y -> (snkPin x) == (snkPin y)) $ sortWith snkPin toNothing
>
>           neutrals     = filter (\x -> (isJust.fst.sourceInfo $ x) 
>                                     && (isJust.fst.sinkInfo   $ x) )    $ edges subG
>
>
>           newIncs      = mergeEdges.unzip.concat.map (\(x, y) -> zip (repeat x) y) $ zip toSubG fromNothing' 
> --        newIncs      = mergeEdges (toSubG, fromNothing)
>           newOuts      = mergeEdges.unzip.concat.map (\(x, y) -> zip x (repeat y)) $ zip toNothing' fromSubG
> --        newOuts      = mergeEdges (toNothing, fromSubG) 





> old_mergeEdges :: ([Edge], [Edge]) -> [Edge]
> old_mergeEdges (xs, ys) 
>     = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
>     where xs' = sortWith snkPin xs
>           ys' = sortWith srcPin ys

> mergeEdges :: ([Edge], [Edge]) -> [Edge]
> mergeEdges (xs, ys)
>     = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
>     where x_snkPins = map snkPin xs
>           y_srcPins = map srcPin ys
>           xs'       = sortWith snkPin $ filter (\edg -> (snkPin edg) `elem` y_srcPins) xs
>           ys'       = sortWith srcPin $ filter (\edg -> (srcPin edg) `elem` x_snkPins) ys


> isSrcPin :: PinID -> Edge -> Bool
> isSrcPin pid (MkEdge (_, pid') (_, _)) = pid == pid'

> isSnkPin :: PinID -> Edge -> Bool
> isSnkPin pid (MkEdge (_, _) (_, pid')) = pid == pid'

> isSrcComp :: CompID -> Edge -> Bool
> isSrcComp cid (MkEdge (Just cid', _) (_, _)) = cid == cid'
> isSrcComp _ _ = False

> isSnkComp :: CompID -> Edge -> Bool
> isSnkComp cid (MkEdge (_, _) (Just cid', _)) = cid == cid'
> isSnkComp _ _ = False

> isToOuter :: Edge -> Bool
> isToOuter (MkEdge (_, _) (Nothing, _)) = True
> isToOuter _                            = False

> isFromOuter :: Edge -> Bool
> isFromOuter (MkEdge (Nothing, _) (_, _)) = True
> isFromOuter _                            = False

> snkPin :: Edge -> PinID
> snkPin (MkEdge (_, _) (_, pid)) = pid

> srcPin :: Edge -> PinID
> srcPin (MkEdge (_, pid) (_, _)) = pid

> snkComp :: Edge -> CompID
> snkComp (MkEdge (_, _) (Just cid, _)) = cid

> srcComp :: Edge -> CompID
> srcComp (MkEdge (Just cid, _) (_, _)) = cid



> isGenerated :: Circuit -> Bool
> isGenerated s = ((== '|').head.name) s && ((== '|').head.reverse.name) s

> isGeneric :: Circuit -> Bool
> isGeneric s = isGenerated s && ((== "|b>c|").name) s

> isID :: Circuit -> Bool
> isID = ((== "-ID-").name)
> -- isID = ((isInfixOf "-ID-").name)


> dropEdgesBordering :: CompID -> [Edge] -> [Edge]
> dropEdgesBordering cid es
>   = (es ++ mergeEdges (toIt, fromIt)) \\ (toIt ++ fromIt)
>   where toIt   = filter ((== (Just cid)).fst.sinkInfo)   $ es
>         fromIt = filter ((== (Just cid)).fst.sourceInfo) $ es

> dropSG :: (Circuit -> Bool) -> Circuit -> Circuit
> dropSG f sg
>   = sg { nodes = newNodes
>        , edges = newEdges
>        }
>   where specific = filter f (nodes sg)
>         newEdges = foldl (flip dropEdgesBordering) (edges sg) (map compID specific)
>         newNodes = map (dropSG f) $ nodes sg \\ specific



> dropGenerated :: Circuit -> Circuit
> dropGenerated = dropSG isGenerated

> dropID :: Circuit -> Circuit
> dropID = dropSG isID


---- Here is another try ----

> allEdges :: Circuit -> [Edge]
> allEdges g = edges g ++ (concat $ map allEdges (nodes g))

> completeEdges :: Circuit -> Circuit 
> completeEdges g 
>     | length (nodes g) == 0
>     = g 
>     | otherwise 
>     = g { nodes = subNodes' }
>     where subNodes  = nodes g
>           subNodes' = map completeEdges $ map (\n -> n { edges = (map (fillEdgeInfoCompID (compID g)) (edges n)) } ) subNodes

> fillEdgeInfoCompID :: CompID -> Edge -> Edge
> fillEdgeInfoCompID cid (MkEdge (Nothing, srcPid) (snkInfo)) = (MkEdge (Just cid, srcPid) (snkInfo))
> fillEdgeInfoCompID cid (MkEdge (srcInfo) (Nothing, snkPid)) = (MkEdge (srcInfo) (Just cid, snkPid))
> fillEdgeInfoCompID _   e = e

> fillSrcInfoCompID :: CompID -> Edge -> Edge
> fillSrcInfoCompID cid (MkEdge (Nothing, srcPid) (snkCid, snkPid))
>     = (MkEdge (Just cid, srcPid) (snkCid, snkPid))

> fillSnkInfoCompID :: CompID -> Edge -> Edge
> fillSnkInfoCompID cid (MkEdge (srcCid, srcPid) (Nothing, snkPid))
>     = (MkEdge (srcCid, srcPid) (Just cid, snkPid))



> getComp :: Circuit -> CompID -> Circuit
> getComp g cid = if length output == 1 
>                   then head output
>                   else error "getComp: corrupted Circuit"
>     where output = getComp' g cid

> getComp' :: Circuit -> CompID -> [Circuit]
> getComp' g cid 
>     | compID g == cid 
>     = [g]
>     | otherwise       
>     = concat $ map (flip getComp' cid) (nodes g)

> isAtomic :: Circuit -> Bool
> isAtomic g
>     = if (length (nodes g) == 0) then True else False



> superNode :: Circuit -> CompID -> Circuit
> superNode g cid 
>    = if length output == 1
>           then head output
>           else error "superNode: corrupted Circuit"
>    where output = superNode' g cid

> superNode' :: Circuit -> CompID -> [Circuit]
> superNode' g cid 
>    | g `isSuperNodeOf` cid
>    = [g]
>    | otherwise
>    = concat $ map (flip superNode' cid) $ nodes g

> isSuperNodeOf :: Circuit -> CompID -> Bool
> isSuperNodeOf g cid 
>     = if length (filter (== cid) subNodes) > 0
>           then True
>           else False
>     where subNodes = map compID $ nodes g

 nextNodes :: Circuit -> CompID -> [Circuit]
 nextNodes g cid 
     = nub $ map ((getComp g).snkComp) $ filter (isSrcComp cid) superEdges
     where superEdges = edges $ superNode (completeEdges g) cid

 nextNodes :: Circuit -> CompID -> [Circuit]
 

> fromComp :: Circuit -> CompID -> [Edge]
> fromComp g cid
>     = filter (\x -> (not.isFromOuter $ x) 
>                  && (cid == (srcComp x) ) ) $ edges $ superNode g cid

> nextAtomic :: Circuit -> Edge -> (CompID, PinID)
> nextAtomic g e
>     | isToOuter e && compID super == 0
>     = (0, snkPin e)
>    
>     | isToOuter e
>     = nextAtomic g $ head $ filter (\x -> sourceInfo x == (Just $ compID super, snkPin e)) $ edges supersuper
>
>     | not.isAtomic $ sub 
>     = nextAtomic g $ head $ filter (\x -> (isFromOuter x) && (snkPin e == srcPin x)) $ edges sub
>
>     | isAtomic sub
>     = (snkComp e, snkPin e)
>     where sub        = getComp   g (snkComp e)
>           super      = superNode g (srcComp e)
>           supersuper = superNode g (compID super)


> flatten :: Circuit -> Circuit
> flatten g 
>     = g' { nodes = nub $ nodes g'
>          , edges =       edges g' ++ enders
>          }
>     where g'            = foldl (f g) (g { nodes = [], edges = starters }) innerConns
> 
>           allAtomCIDs   = filter isAtomic $ allCompIDs g
>           edgsFromAtom  = map (fromComp g . compID) allAtomCIDs
>           nextCIDs      = map (map $ nextAtomic g) edgsFromAtom
>           connections   = concat $ map (\(x, ys) -> concat $ map (\z -> [(compID x, z)]) ys) $ zip allAtomCIDs nextCIDs
>
>           starters      = zipWith (\(c, p)      i -> MkEdge (Nothing, i) (Just c,  p)) (map (nextAtomic g) fromOuterEs)                 ([0..])
>           enders        = zipWith (\(c, (_, p)) i -> MkEdge (Just c,  p) (Nothing, i)) (filter (\x  -> (fst.snd) x == 0) $ connections) ([0..])
> 
>           innerConns    = filter (\x  -> (fst.snd) x /= 0) $ connections
>           fromOuterEs   = filter (isFromOuter) $ edges g
>
>           f orig_g new_g (nextF, nextTo@(nextToC, nextToP)) 
>                         = new_g { nodes = nub $ nodes new_g ++ (getComp g nextF) : [getComp g nextToC]
>                                 , edges = nub $ edges new_g ++ [(connectCID orig_g new_g nextF nextTo)]
>                                 } 


> connectCID :: Circuit -> Circuit -> CompID -> (CompID, PinID) -> Edge
> connectCID old_g g cidF (cidT,pidT)
>     = MkEdge (Just cidF, nextFpin) (Just cidT, pidT)
>     where nextFpin  = head $ drop cntEsFrom $ sources $ getComp old_g cidF
>           cntEsFrom = length $ filter (\x -> (not.isFromOuter $ x) && (srcComp x == cidF)) $ edges g
