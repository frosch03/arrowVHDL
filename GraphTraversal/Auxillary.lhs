> module GraphTraversal.Auxillary 
>     ( connect
>     , combine
>     , mkPins
>     , flatten
>     , allCompIDs
>     , getAllNodes
>     , getAtomicNodes
>     , getAllEdges
>     , getEdgeTo
>     , getEdgeFrom
>     , conflate
>     , conflateEdge
>     , conflateEdges
>     , t
>     )
> where

> import Control.Arrow

> import Data.List (union, groupBy, (\\))
> import Data.Maybe 
> import Data.Either
> import Control.Monad (msum)

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
>                                   , nodes   = sg_f': sg_g' : []
>                                   , edges   = es
>                                   , sinks   = srcs 
>                                   , sources = snks
>                                   }
>     where sg_f'              = alterCompIDs 1                    sg_f
>           sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
>           (es, (srcs, snks)) = rewire sg_f' sg_g'

TODO: sg_f' (with edges from outside) vs. sg_f'' (only the inner edges)
I'm not sure right now, if the edges from the outside are needed.
therefore at the moment, the new nodes are generated from sg_f' and sg_g' 
           sg_f''             = sg_f' { edges = onlyInnerEdges $ edges sg_f'}
           sg_g''             = sg_g' { edges = onlyInnerEdges $ edges sg_g' }


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
> mkPins 0 = error $ show "It is not possible to generate a component with 0 pins"
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


> samePin :: Edge -> Edge -> Bool
> samePin (MkEdge (_, ip1) (_, op1))
>         (MkEdge (_, ip2) (_, op2))
>     =  op1 == ip2
>     || op2 == ip1


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

> mergeEdge :: Edge -> Edge -> Edge
> mergeEdge (MkEdge from _) (MkEdge _ to) = MkEdge from to

 



> t = flatten

> flatten :: StructGraph -> StructGraph 
> flatten g = g' { nodes = atomgraphs ++ (concat $ map nodes subgraphs)
>                , edges = (((edges g) \\ delEs) ++ newEs ++ neutralEs)  
>                }
>     where g'         = g { nodes = map flatten (nodes g) } 
>           subgraphs  = filter (not.null.nodes) $ nodes g' 
>           atomgraphs = filter (    null.nodes) $ nodes g'
>           allEs      = map (xxx g') subgraphs
>           newEss     = map fst allEs
>           newEs      = concat $ map fst newEss
>           neutralEs  = concat $ map snd newEss
>           delEs      = concat $ map snd allEs

> xxx :: StructGraph -> StructGraph -> (([Edge], [Edge]), [Edge])
> xxx superG subG = ( ( newIncs ++ newOuts
>                     , neutrals
>                     )
>                   , toSubG  ++ fromSubG
>                   )
>     where toSubG      = filter ((==Just (compID subG)).fst.sinkInfo) $ edges superG
>           fromNothing = filter (isNothing.fst.sourceInfo)            $ edges subG
>           newIncs     = mergeEdges (toSubG, fromNothing)
>
>           toNothing   = filter (isNothing.fst.sinkInfo)                $ edges subG
>           fromSubG    = filter ((==Just (compID subG)).fst.sourceInfo) $ edges superG
>           newOuts     = mergeEdges (toNothing, fromSubG) 
>
>           neutrals    = filter (\x -> (isJust.fst.sourceInfo $ x) 
>                                    && (isJust.fst.sinkInfo   $ x) )  $ edges subG

> mergeIncomingEdges :: ([Edge], [Edge]) -> [Edge]
> mergeIncomingEdges (e1, e2) | length e2 < length e1 = error $ "to few outer edges"
> mergeIncomingEdges (_, [])                          = []
> mergeIncomingEdges (outers, inner:inners) 
>    = (MkEdge (sourceInfo outer) (sinkInfo inner)) : (mergeIncomingEdges (outers \\ [outer], inners \\ [inner]))
>    where outer = head $ filter (isSnkPin (srcPin inner)) outers

> mergeOutgoingEdges :: ([Edge], [Edge]) -> [Edge]
> mergeOutgoingEdges (e1, e2) | length e2 < length e1 = error $ "to few outer edges"
> mergeOutgoingEdges (_, [])                          = []
> mergeOutgoingEdges (inner:inners, outers) 
>    = (MkEdge (sourceInfo inner) (sinkInfo outer)) : (mergeOutgoingEdges (inners \\ [inner], outers \\ [outer]))
>    where outer = head $ filter (isSrcPin (snkPin inner)) outers

> mergeEdges :: ([Edge], [Edge]) -> [Edge]
> mergeEdges (xs, ys) 
>     = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
>     where xs' = sortBySnkPin xs
>           ys' = sortBySrcPin ys


> mergeIncEdges :: ([Edge], [Edge]) -> [Edge]
> mergeIncEdges (outers, inners) 
>     = zipWith (\outer inner -> MkEdge (sourceInfo outer) (sinkInfo inner)) outers' inners'
>     where outers' = sortBySnkPin outers
>           inners' = sortBySrcPin inners

> mergeOutEdges :: ([Edge], [Edge]) -> [Edge]
> mergeOutEdges (inners, outers) 
>     = zipWith (\inner outer -> MkEdge (sourceInfo inner) (sinkInfo outer)) inners' outers'
>     where inners' = sortBySnkPin inners
>           outers' = sortBySrcPin outers


> isSrcPin :: PinID -> Edge -> Bool
> isSrcPin pid (MkEdge (_, pid') (_, _)) = pid == pid'

> isSnkPin :: PinID -> Edge -> Bool
> isSnkPin pid (MkEdge (_, _) (_, pid')) = pid == pid'

> snkPin :: Edge -> PinID
> snkPin (MkEdge (_, _) (_, pid)) = pid

> srcPin :: Edge -> PinID
> srcPin (MkEdge (_, pid) (_, _)) = pid

> sortByPin :: (Edge -> PinID) -> [Edge] -> [Edge]
> sortByPin _ [] = []
> sortByPin f (e:es) 
>     =        (sortByPin f [x | x <- es, (f x <= f e)]) 
>       ++ e : (sortByPin f [y | y <- es, (f y >  f e)])

> sortBySrcPin :: [Edge] -> [Edge]
> sortBySrcPin = sortByPin srcPin

> sortBySnkPin :: [Edge] -> [Edge]
> sortBySnkPin = sortByPin snkPin


> superNode :: StructGraph -> CompID -> Maybe CompID
> superNode g cid | ((==0).length.nodes) g              = Nothing
> superNode g cid | (elem cid) ((map compID) (nodes g)) = Just $ compID g
> superNode g cid | otherwise                           = msum $ map (\x -> superNode x cid) $ nodes g 

> atomicNodes :: StructGraph -> [CompID]
> atomicNodes g | ((==0).length.nodes) g = [compID g]
> atomicNodes g | otherwise              = concat $ map atomicNodes (nodes g)

> deletableNodes :: StructGraph -> [CompID]
> deletableNodes g = allCompIDs g \\ (compID g : atomicNodes g)


> toFollow :: StructGraph -> [Edge]
> toFollow g = startEs ++ fromAtomEs
>     where allEs       = (fst.conflate $ g) ++ (snd.conflate $ g)
>           without x y = not $ y `elem` x
>           delAble     = deletableNodes g
>           startEs     = filter                (isNothing.fst.sourceInfo)  allEs
>           fromAtomEs  = filter (without delAble.fromJust.fst.sourceInfo) (allEs \\ startEs) 

> conflateEdges :: [Edge] -> ([Edge], [Edge]) -> [Edge]
> conflateEdges e0s (esL, esR) = map (\x -> conflateEdge x (esL)) e0s

> conflateEdge :: Edge -> [Edge] -> Edge
> conflateEdge e0 (e:es) | e `isNextEdgeOf` e0 = conflateEdge (mergeEdge e0 e) es
> conflateEdge e0 (e:es) | otherwise           = conflateEdge e0 es
> conflateEdge e0 []                           = e0 

> isNextEdgeOf :: Edge -> Edge -> Bool
> isNextEdgeOf (MkEdge (Just iCid, iPid) _) (MkEdge _ (Just oCid, oPid))
>     = oCid == iCid && oPid == iPid
> isNextEdgeOf (MkEdge (Nothing, _) _) _ = False
> isNextEdgeOf _ (MkEdge _ (Nothing, _)) = False

> isPrevEdgeOf :: Edge -> Edge -> Bool
> isPrevEdgeOf (MkEdge _ (Just oCid, oPid)) (MkEdge (Just iCid, iPid) _)
>     = oCid == iCid && oPid == iPid
> isPrevEdgeOf (MkEdge _ (Nothing, _)) _ = False
> isPrevEdgeOf _ (MkEdge (Nothing, _) _) = False

> toFollowFrom :: StructGraph -> [Edge]
> toFollowFrom g = endEs ++ toAtomEs 
>     where allEs       = (fst.conflate $ g) ++ (snd.conflate $ g) 
>           endEs       = filter (isNothing.fst.sinkInfo) allEs
>           without x y = not $ y `elem` x 
>           delAble     = deletableNodes g 
>           toAtomEs    = filter (without delAble.fromJust.fst.sinkInfo) (allEs \\ endEs) 






> connFromAtom :: StructGraph -> (CompID, PinID) -> SourceAnchor
> connFromAtom g (cid, pid) | ((==0).length.nodes) g = (Just cid, pid)
> connFromAtom g (cid, pid) | otherwise 
>     = connFromAtom g' (cid', pid') 
>     where g1 = getGraph g cid
>           _  = if (isOutPin g1 pid) 
>                   then fromJust . getMaybePin sources g $ pid
>                   else error $ show "There is no such Anchor"
> 
>           (g', (cid', pid')) = getNextHop g (cid, pid)

> getNextHop :: StructGraph -> (CompID, PinID) -> (StructGraph, (CompID, PinID))
> getNextHop g (cid, pid) = (getGraph g nextCid, (nextCid, nextPid))
>     where (MkEdge (Just nextCid, nextPid) _) = head $ filter (\(MkEdge _ (Just tCid, tPid)) 
>                                                              -> tCid == cid && tPid == pid) $ getAllEdges g


> isOutPin :: StructGraph -> PinID -> Bool
> isOutPin g pid = if isNothing (getMaybePin sources g pid) 
>                     then False
>                     else True

> isInPin :: StructGraph -> PinID -> Bool
> isInPin g pid = if isNothing (getMaybePin sinks g pid)
>                    then False
>                    else True



> getMaybePin :: (StructGraph -> Pins)
>             -> StructGraph -> PinID -> Maybe PinID
> getMaybePin f g pid = listToMaybe . filter (==pid) $ f g 





> getGraph :: StructGraph -> CompID -> StructGraph 
> getGraph g cid 
>     = fromMaybe (error $ show "Sorry, there is no such Graph") 
>                 (getMaybeGraph g cid)
                                     
> getMaybeGraph :: StructGraph -> CompID -> Maybe StructGraph
> getMaybeGraph g cid 
>     = listToMaybe . filter (\n -> compID n == cid) $ getAllGraphs g 

> getGraphs :: (StructGraph -> [StructGraph]) 
>           -> (StructGraph -> [StructGraph])
>           -> StructGraph -> [StructGraph]
> getGraphs f f' g | ((==0).length.nodes) g = (f' g)
> getGraphs f f' g | otherwise              = (f  g) ++ (concat $ map (getGraphs f f') $ nodes g)

> getAllGraphs :: StructGraph -> [StructGraph]
> getAllGraphs = getGraphs (:[]) (:[])

> getAllNodes :: StructGraph -> [StructGraph]
> getAllNodes = getGraphs (\n -> [n { nodes = [] }]) (:[])

> getAtomicNodes :: StructGraph -> [StructGraph]
> getAtomicNodes = getGraphs (\n -> []) (:[])




> getAllEdges :: StructGraph -> [Edge]
> getAllEdges g | ((==0).length.edges) g = []      ++ (concat $ map getAllEdges $ nodes g)
> getAllEdges g | otherwise              = edges g ++ (concat $ map getAllEdges $ nodes g)



> conflate :: StructGraph -> ([Edge], [Edge])
> conflate g = (leftEs ++ left_ ++ rest_, rightEs ++ right_)
>     where left_   = filter (isNothing.fst.sourceInfo) $ edges g 
>           right_  = filter (isNothing.fst.sinkInfo)   $ edges g
>           rest_   = (edges g) \\ (left_ ++ right_)
>           (leftEs, rightEs) = (partitionEithers . concat $ map conflate' (nodes g) )

> conflate' :: StructGraph -> [Either Edge Edge]
> conflate' g | ((==0).length.nodes) g = map (normalizeEdge $ compID g) (edges g)
> conflate' g | otherwise              = map (normalizeEdge $ compID g) (edges g) ++ (concat (map conflate' (nodes g)))

> normalizeEdge :: CompID -> Edge -> Either Edge Edge
> normalizeEdge cid (MkEdge (Nothing, fromPID) to) = Left  $ MkEdge (Just cid, fromPID) to
> normalizeEdge cid (MkEdge from (Nothing, toPID)) = Right $ MkEdge from (Just cid, toPID)
> normalizeEdge cid e@(MkEdge (Just fCid, _) _) | fCid == cid = Left  e
> normalizeEdge cid e@(MkEdge _ (Just tCid, _)) | tCid == cid = Right e
> normalizeEdge cid e                                         = Left  e




> getEdgeFrom :: StructGraph -> (CompID, PinID) -> Edge
> getEdgeFrom g (cid, pid) 
>     = head $ filter f $ snd $ conflate g
>     where f (MkEdge (Just x, y)  _) = cid == x && pid == y 
>           f (MkEdge (Nothing, _) _) = False

> getEdgeTo :: StructGraph -> (CompID, PinID) -> Edge
> getEdgeTo g (cid, pid) 
>     = head $ filter f $ fst $ conflate g
>     where f (MkEdge _ (Just x,  y)) = cid == x && pid == y
>           f (MkEdge _ (Nothing, _)) = False

