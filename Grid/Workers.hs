module Grid.Workers
where

import Data.List (nub, (\\))

import GHC.Exts (sortWith)

import Grid.Core
import Grid.Sensors
import Grid.Tests


-- Circuit Workers
--

alterCompIDs :: Int -> Circuit -> Circuit
alterCompIDs i sg 
    = sg { compID = compID sg + i
         , nodes  = map (alterCompIDs i) $ nodes sg
         , edges  = map (\ (MkEdge (ci,pi) (co,po)) 
                        -> (MkEdge (maybe ci (Just.(+i)) $ ci ,pi) 
                                   (maybe co (Just.(+i)) $ co ,po))
                        ) $ edges sg
         }


dropCircuit :: (Circuit -> Bool) -> Circuit -> Circuit
dropCircuit f sg
  = sg { nodes = newNodes
       , edges = newEdges
       }
  where specific = filter f (nodes sg)
        newEdges = foldl (flip dropEdgesBordering) (edges sg) (map compID specific)
        newNodes = map (dropCircuit f) $ nodes sg \\ specific


flatten :: Circuit -> Circuit
flatten g 
    = g' { nodes = nub $ nodes g'
         , edges =       edges g' ++ enders
         }
    where g'            = foldl (f g) (g { nodes = [], edges = starters }) innerConns

          allAtomCIDs   = filter isAtomic $ allCircuits g
          edgsFromAtom  = map (fromCompEdges g . compID) allAtomCIDs
          nextCIDs      = map (map $ nextAtomic g) edgsFromAtom
          connections   = concat $ map (\(x, ys) -> concat $ map (\z -> [(compID x, z)]) ys) $ zip allAtomCIDs nextCIDs

          starters      = zipWith (\(c, p)      i -> MkEdge (Nothing, i) (Just c,  p)) (map (nextAtomic g) fromOuterEs)                 ([0..])
          enders        = zipWith (\(c, (_, p)) i -> MkEdge (Just c,  p) (Nothing, i)) (filter (\x  -> (fst.snd) x == 0) $ connections) ([0..])

          innerConns    = filter (\x  -> (fst.snd) x /= 0) $ connections
          fromOuterEs   = filter (isFromOuter) $ edges g

          f orig_g new_g (nextF, nextTo@(nextToC, nextToP)) 
                        = new_g { nodes = nub $ nodes new_g ++ (getComp g nextF) : [getComp g nextToC]
                                , edges = nub $ edges new_g ++ [(connectCID orig_g new_g nextF nextTo)]
                                } 

dropGenerated :: Circuit -> Circuit
dropGenerated = dropCircuit isGenerated

dropID :: Circuit -> Circuit
dropID = dropCircuit isID




-- Edge Workers
--

connectCID :: Circuit -> Circuit -> CompID -> (CompID, PinID) -> Edge
connectCID old_g g cidF (cidT,pidT)
    = MkEdge (Just cidF, nextFpin) (Just cidT, pidT)
    where nextFpin  = head $ drop cntEsFrom $ sources $ getComp old_g cidF
          cntEsFrom = length $ filter (\x -> (not.isFromOuter $ x) && (srcComp x == cidF)) $ edges g

dropEdgesBordering :: CompID -> [Edge] -> [Edge]
dropEdgesBordering cid es
  = (es ++ mergeEdges (toIt, fromIt)) \\ (toIt ++ fromIt)
  where toIt   = filter ((== (Just cid)).fst.sinkInfo)   $ es
        fromIt = filter ((== (Just cid)).fst.sourceInfo) $ es

mergeEdges :: ([Edge], [Edge]) -> [Edge]
mergeEdges (xs, ys)
    = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
    where x_snkPins = map snkPin xs
          y_srcPins = map srcPin ys
          xs'       = sortWith snkPin $ filter (\edg -> (snkPin edg) `elem` y_srcPins) xs
          ys'       = sortWith srcPin $ filter (\edg -> (srcPin edg) `elem` x_snkPins) ys

fillEdgeInfoCompID :: CompID -> Edge -> Edge
fillEdgeInfoCompID cid (MkEdge (Nothing, srcPid) (snkInfo)) = (MkEdge (Just cid, srcPid) (snkInfo))
fillEdgeInfoCompID cid (MkEdge (srcInfo) (Nothing, snkPid)) = (MkEdge (srcInfo) (Just cid, snkPid))
fillEdgeInfoCompID _   e = e

fillSrcInfoCompID :: CompID -> Edge -> Edge
fillSrcInfoCompID cid (MkEdge (Nothing, srcPid) (snkCid, snkPid))
    = (MkEdge (Just cid, srcPid) (snkCid, snkPid))

fillSnkInfoCompID :: CompID -> Edge -> Edge
fillSnkInfoCompID cid (MkEdge (srcCid, srcPid) (Nothing, snkPid))
    = (MkEdge (srcCid, srcPid) (Just cid, snkPid))

