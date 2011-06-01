> module GraphTraversal.Auxillary 
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


> drop_first  :: (Arrow a) => a (b, b') b'
> drop_first  =  arr (\(x, y) -> y)

> drop_second :: (Arrow a) => a (b, b') b
> drop_second =  arr (\(x, y) -> x)




> flatten :: StructGraph -> StructGraph 
> flatten g = g' { nodes = atomgraphs ++ (concat $ map nodes subgraphs)
>                , edges = (((edges g) \\ delEs) ++ newEs ++ neutralEs)  
>                }
>     where g'         = g { nodes = map flatten (nodes g) } 
>           subgraphs  = filter (not.null.nodes) $ nodes g' 
>           atomgraphs = filter (    null.nodes) $ nodes g'
>           allEs      = map (partitionEdges g') subgraphs
>           newEss     = map fst allEs
>           newEs      = concat $ map fst newEss
>           neutralEs  = concat $ map snd newEss
>           delEs      = concat $ map snd allEs



> partitionEdges :: StructGraph -> StructGraph -> (([Edge], [Edge]), [Edge])
> partitionEdges superG subG  = ( ( newIncs ++ newOuts
>                                 , neutrals
>                                 )
>                               , toSubG  ++ fromSubG
>                               )
>     where fromSubG    = filter ((==Just (compID subG)).fst.sourceInfo) $ edges superG
>           toSubG      = filter ((==Just (compID subG)).fst.sinkInfo)   $ edges superG
>
>           fromNothing = filter (isNothing.fst.sourceInfo)              $ edges subG
>           toNothing   = filter (isNothing.fst.sinkInfo)                $ edges subG
>
>           neutrals    = filter (\x -> (isJust.fst.sourceInfo $ x) 
>                                    && (isJust.fst.sinkInfo   $ x) )    $ edges subG
>
>           newIncs     = mergeEdges (toSubG, fromNothing)
>           newOuts     = mergeEdges (toNothing, fromSubG) 



> mergeEdges :: ([Edge], [Edge]) -> [Edge]
> mergeEdges (xs, ys) 
>     = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
>     where xs' = sortBySnkPin xs
>           ys' = sortBySrcPin ys



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
