> module Grid.Auxillary 
> where

> -- import Control.Arrow hiding (Arrow)

> import Data.List (union, groupBy, isInfixOf)
> import Data.Maybe 
> import Data.Either
> import Control.Monad (msum)

> import GHC.Exts (sortWith)

> import Grid.Core
> import Grid.Show
> import Grid.Tests
> import Grid.Splice
> import Grid.Sensors
> import Grid.Workers


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

> connect :: Circuit -> Circuit -> Circuit
> connect = splice (seqRewire, ">>>")


TODO combine = frame???

> combine :: Circuit -> Circuit -> Circuit
> combine = splice (parRewire, "&&&")

> dupCombine :: Circuit -> Circuit -> Circuit
> dupCombine = splice (dupParRewire, ">2>")


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




---- Here is another try ----


--- Functions that obviously do not belong here: ---

> collNext :: Int -> Int
> collNext n = if n == 1 
>                   then 1 
>                   else (if (even n) 
>                             then (n `div` 2) 
>                             else (3*n + 1)
>                        )

> collatz :: ((Int, Int) -> Int) -> (Int, Int) -> Int
> collatz = (\f (n, step) 
>           -> if n == 1 
>                   then step 
>                   else f (collNext n, step +1)
>           )

> or_else x1 x2 = if x1 then x1 else x2
