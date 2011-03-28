> module GraphTraversal.Auxillary 
>     ( connect
>     , combine
>     , mkPins
>     , flatten
>     , allCompIDs
>     )
> where

> import Control.Arrow

> import Data.List (union, groupBy, (\\))

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
> combine up down = MkSG { name    = (name up') ++ "***" ++ (name down') 
>                        , compID  = newCompID
>                        , nodes   = up' : down' : []
>                        , edges   = edgs
>                        , sinks   = super_snks
>                        , sources = super_srcs
>                        }
>     where [up', down'] =  fst.unifyCompIDs $ ([up, down], 0)
>           newCompID    =  nextID $ (allCompIDs up') ++ (allCompIDs down')
>           super_snks   =  [0..(length.sources $ up') + (length.sources $ down')-1]
>           super_srcs   =  [0..(length.sinks   $ up') + (length.sinks   $ down')-1]
>           edgs         =  let len_snks_up   = (length.sinks   $ up')
>                               len_srcs_down = (length.sources $ down')
>                           in (fst $ wire (Just $ newCompID)    (Just $ compID up')   (super_srcs)                  (sinks up'))
>                           ++ (fst $ wire (Just $ newCompID)    (Just $ compID down') (drop len_snks_up super_srcs) (sinks down'))
>                           ++ (fst $ wire (Just $ compID up')   (Just $ newCompID)    (sources down')               (super_snks))
>                           ++ (fst $ wire (Just $ compID down') (Just $ newCompID)    (sources down')               (drop len_srcs_down super_snks))

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
>     where (edgs, (srcs_l', snks_r')) =  wire (Just $ compID sg_l) (Just $ compID sg_r) (sources sg_l) (sinks sg_r)
>           super_srcs                 =  [0..(length.sinks   $ sg_l) + length snks_r' -1]
>           super_snks                 =  [0..(length.sources $ sg_r) + length srcs_l' -1]
>           src_edges                  =  let len_snks_l = (length.sinks $ sg_l)
>                                         in (fst $ wire Nothing (Just $ compID sg_l) super_srcs                   (sinks sg_l))
>                                         ++ (fst $ wire Nothing (Just $ compID sg_r) (drop len_snks_l super_srcs) (sinks sg_r))
>           snk_edges                  =  let len_srcs_r = (length.sources $ sg_r)
>                                         in (fst $ wire (Just $ compID sg_r) Nothing (sources sg_r) (super_snks))
>                                         ++ (fst $ wire (Just $ compID sg_l) Nothing (sources sg_l) (drop len_srcs_r super_snks))


> wire :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
> wire cid_l cid_r pins_l pins_r 
>     = (edges, (drop cnt pins_l, drop cnt pins_r))
>     where points_l = map ((,) (cid_l)) pins_l
>           points_r = map ((,) (cid_r)) pins_r
>           edges    = map (uncurry MkEdge) $ zip points_l points_r
>           cnt      = length edges



> unifyCompID :: (StructGraph, CompID) -> (StructGraph, CompID)
> unifyCompID (sg, cid) 
>     = ( sg { compID = cid
>            , nodes  = sub_sg'
>            , edges  = new_edges
>            }
>       , cid_next
>       )
>     where (sub_sg', cid_next) = unifyCompIDs (nodes sg, cid+1)
>           old_cid             = compID sg
>           new_edges           = map (fitEdge (compID sg, cid)) $ edges sg


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


> fitEdges  :: ([Edge], (CompID, CompID)) -> [Edge]
> fitEdges (edges, (old_cid, new_cid)) 
>     = map (fitEdge (old_cid, new_cid)) edges

> fitEdge :: (CompID, CompID) -> Edge -> Edge
> fitEdge (o, n) (MkEdge (Just c, p) s) = error $ show (o,n) ++ "1: " ++ (show $ MkEdge (if o == c then Just n else Just c, p) s)
> fitEdge (o, n) (MkEdge s (Just c, p)) = error $ show (o,n) ++ "2: " ++ (show $ MkEdge s (if o == c then Just n else Just c, p))
> fitEdge _      _                      = error $ "3: " ++ "this should't happen"

> mkPins :: Int -> Pins
> mkPins 0 = []
> mkPins n = [0..n-1]

> nextID :: [CompID] -> CompID
> nextID []    = 0
> nextID [cid] = cid + 1
> nextID cids  = nextID [foldl max 0 cids]

  allCompIDs :: StructGraph -> [CompID]
  allCompIDs g | length.nodes $ g >  2 = compID g : (map allCompIDs) $ nodes g
               | length.nodes $ g == 1 = compID g : allCompIDs $ head.nodes $ g
               | otherwise             = compID g : []


> fromOrToComps :: Edge -> [CompID] -> Bool
> fromOrToComps e cs = foldl (&&) True $ map (fromOrToComp e) cs

> fromOrToComp :: Edge -> CompID -> Bool
> fromOrToComp edg cid 
>     =  fromComp edg cid
>     || toComp   edg cid

> fromComp, toComp :: Edge -> CompID -> Bool
> fromComp (MkEdge (Just fromID,_) _)        cid = cid == fromID
> toComp   (MkEdge _          (Just toID,_)) cid = cid == toID

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

> mergeEdges :: [Edge] -> CompID -> [Edge]
> mergeEdges es mcid = (es \\ tofrom) ++ (map mergeEdge2 $ groupBy samePin tofrom)
>     where tofrom = [ xs | xs <- es, xs `toComp` mcid || xs `fromComp` mcid ]

           f :: ([Edge], [Edge]) -> ([Edge], [Edge])
           f ([],    fines) = ([], fines)
           f (oldes, fines) = (oldes \\ newes, newes ++ fines)
               where newes = map mergeEdge $ groupBy samePin oldes


> missingCIDs :: StructGraph -> StructGraph -> [CompID]
> missingCIDs g1 g2 = [ missing | missing <- cs_super, not $ missing `elem` cs_sub]
>     where (cs_super, cs_sub) = if length cids1 > length cids2 
>                                   then (cids1, cids2) 
>                                   else (cids2, cids1)
>           cids1              = allCompIDs g1
>           cids2              = allCompIDs g2

> flatten :: StructGraph -> StructGraph 
> flatten g = g' { edges = es' }
>     where (gs, es) = flatten' (nodes g, edges g)
>           g'       = g { nodes = gs, edges = es }
>           es'      = foldl union [] $ map (mergeEdges es) $ (missingCIDs g g')

> flatten' :: ([StructGraph], [Edge]) -> ([StructGraph], [Edge])
> flatten' ([], e)  = ([], e)
> flatten' ((g:gs), es) | (length.nodes) g > 1 = flatten' (nodes g { edges = [] }, es `union` edges g)
> flatten' ((g:gs), es) | otherwise            = (g : gs' , es `union` es')
>     where (gs', es') = flatten' (gs, es)



> drop_first  :: (Arrow a) => a (b, b') b'
> drop_first  =  arr (\(x, y) -> y)

> drop_second :: (Arrow a) => a (b, b') b
> drop_second =  arr (\(x, y) -> x)
