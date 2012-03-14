module Grid.Sensors
where

import Grid.Core
import Grid.Tests

-- Circuit Sensors:
--

allCircuits :: CircuitDescriptor -> [CircuitDescriptor]
allCircuits sg 
    = if (length next_sg == 0) then sg : []
                               else sg : (concat $ map allCircuits next_sg)
    where next_sg = nodes sg
          cid     = compID sg 

getComp :: CircuitDescriptor -> CompID -> CircuitDescriptor
getComp g cid = if length output == 1 
                  then head output
                  else error "getComp: corrupted CircuitDescriptor"
    where output = getComp' g cid

getComp' :: CircuitDescriptor -> CompID -> [CircuitDescriptor]
getComp' g cid 
    | compID g == cid 
    = [g]
    | otherwise       
    = concat $ map (flip getComp' cid) (nodes g)

maxCompID :: CircuitDescriptor -> CompID
maxCompID sg = compID sg `max` (foldl max 0 $ map maxCompID (nodes sg))


superNode :: CircuitDescriptor -> CompID -> CircuitDescriptor
superNode g cid 
   = if length output == 1
          then head output
          else error "superNode: corrupted CircuitDescriptor"
   where output = superNode' g cid

superNode' :: CircuitDescriptor -> CompID -> [CircuitDescriptor]
superNode' g cid 
   | g `isSuperNodeOf` cid
   = [g]
   | otherwise
   = concat $ map (flip superNode' cid) $ nodes g

nextAtomic :: CircuitDescriptor -> Edge -> (CompID, PinID)
nextAtomic g e
    | isToOuter e && compID super == 0
    = (0, snkPin e)
   
    | isToOuter e
    = nextAtomic g $ head $ filter (\x -> sourceInfo x == (Just $ compID super, snkPin e)) $ edges supersuper

    | not.isAtomic $ sub 
    = nextAtomic g $ head $ filter (\x -> (isFromOuter x) && (snkPin e == srcPin x)) $ edges sub

    | isAtomic sub
    = (snkComp e, snkPin e)
    where sub        = getComp   g (snkComp e)
          super      = superNode g (srcComp e)
          supersuper = superNode g (compID super)


allEdges :: CircuitDescriptor -> [Edge]
allEdges g = edges g ++ (concat $ map allEdges (nodes g))

fromCompEdges :: CircuitDescriptor -> CompID -> [Edge]
fromCompEdges g cid
    = filter (\x -> (not.isFromOuter $ x) 
                 && (cid == (srcComp x) ) ) $ edges $ superNode g cid


-- Edge Sensors
--

snkPin :: Edge -> PinID
snkPin (MkEdge (_, _) (_, pid)) = pid

srcPin :: Edge -> PinID
srcPin (MkEdge (_, pid) (_, _)) = pid

snkComp :: Edge -> CompID
snkComp (MkEdge (_, _) (Just cid, _)) = cid

srcComp :: Edge -> CompID
srcComp (MkEdge (Just cid, _) (_, _)) = cid

