module Grid.Tests 
where

import Grid.Core

isFromOrToComp :: CompID -> Edge -> Bool
isFromOrToComp cid (MkEdge (Nothing, pi) (Just co, po)) = cid == co
isFromOrToComp cid (MkEdge (Just ci, pi) (Nothing, po)) = cid == ci
isFromOrToComp cid (MkEdge (Just ci, pi) (Just co, po)) = cid == co 

isFromOutside :: Edge -> Bool
isFromOutside (MkEdge (Nothing, _) _) = True
isFromOutside otherwise               = False

isToOutside :: Edge -> Bool
isToOutside (MkEdge _ (Nothing, _)) = True
isToOutside otherwise               = False

hasLabel :: String -> Circuit -> Bool
hasLabel s
    = ((== s).label)

isID :: Circuit -> Bool
isID = hasLabel "-ID-"

isAtomic :: Circuit -> Bool
isAtomic g
    = if (length (nodes g) == 0) then True else False

isSuperNodeOf :: Circuit -> CompID -> Bool
isSuperNodeOf g cid 
    = if length (filter (== cid) subNodes) > 0
          then True
          else False
    where subNodes = map compID $ nodes g

isToOuter :: Edge -> Bool
isToOuter (MkEdge (_, _) (Nothing, _)) = True
isToOuter _                            = False

isFromOuter :: Edge -> Bool
isFromOuter (MkEdge (Nothing, _) (_, _)) = True
isFromOuter _                            = False

isGenerated :: Circuit -> Bool
isGenerated s = ((== '|').head.label) s && ((== '|').head.reverse.label) s



-- This test (isFromComp) is never used
isFromComp :: CompID -> Edge -> Bool 
isFromComp cid (MkEdge (Just ci, _) _) = cid == ci
isFromComp _   _                       = False

-- This test (isToComp) is never used
isToComp :: CompID -> Edge -> Bool 
isToComp cid (MkEdge _ (Just co, _)) = cid == co
isToComp _   _                       = False

-- This test (isIOPort) is never used
isIOPort :: (String, Anchor) -> Bool
isIOPort (_, (Nothing, _)) = True
isIOPort otherwise         = False

-- This test (isAtComp) is never used
isAtComp :: CompID -> (String, Anchor) -> Bool
isAtComp cid (_, (Just cid', _)) 
    = cid == cid'

-- This test (isSrcPin) is never used
isSrcPin :: PinID -> Edge -> Bool
isSrcPin pid (MkEdge (_, pid') (_, _)) = pid == pid'

-- This test (isSnkPin) is never used
isSnkPin :: PinID -> Edge -> Bool
isSnkPin pid (MkEdge (_, _) (_, pid')) = pid == pid'

-- This test (isSrcComp) is never used
isSrcComp :: CompID -> Edge -> Bool
isSrcComp cid (MkEdge (Just cid', _) (_, _)) = cid == cid'
isSrcComp _ _ = False

-- This test (isSnkComp) is never used
isSnkComp :: CompID -> Edge -> Bool
isSnkComp cid (MkEdge (_, _) (Just cid', _)) = cid == cid'
isSnkComp _ _ = False

-- This test (isGeneric) is never used
isGeneric :: Circuit -> Bool
isGeneric s = isGenerated s && ((== "|b>c|").label) s
