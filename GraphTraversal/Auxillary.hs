module GraphTraversal.Auxillary 
    ( connect
    , combine
    , newCompID
    , drop_first
    , drop_second
    )
where

import Control.Arrow

import Data.Either ( lefts
                   , rights
                   )
import Data.Maybe  ( fromJust
                   , isNothing
                   )

import GraphTraversal.Core

connect :: StructGraph -> StructGraph -> StructGraph
connect left right = MkSG { name    = (name left ++ ">>>" ++ name right)
                          , compID  = nextID $ (collectCompIDs left) ++ 
                                               (collectCompIDs right)
                          , node    = connSG (node left) (node right)
                          , edges   = newedges ++ edges left ++ edges right
                          , sinks   = sinks left
                          , sources = sources right
                          }
    where sources_l = map snd $ filter isOuterEdge $ sources left
          sinks_r   = map snd $ filter isOuterEdge $ sinks right

          newedges = zipWith (\src snk -> MkEdge (Just $ compID left , src) 
                                                 (Just $ compID right, snk)) sources_l sinks_r


newEdges :: (Maybe CompID, Maybe CompID) -> [PinID] -> [PinID] -> [Edge]
newEdges (cid_l, cid_r) = zipWith (\src snk -> MkEdge (cid_l, src) 
                                                      (cid_r, snk))

outerPins :: Connection -> [PinID]
outerPins ops = map snd $ filter isOuterEdge $ ops

connSG :: (CompID, (CompID, CompID)) -> StructGraph -> Maybe StructGraph -> Maybe StructGraph -> (Maybe StructGraph, [Edge])
connSG (cid_super, (cid_l, cid_r)) sg Nothing        Nothing        = (Nothing, newEdges (Nothing, Nothing) (sources sg) (sinks sg))
connSG (cid_super, (cid_l, cid_r)) sg (Just inner_l) Nothing        = (
connSG (cid_super, (cid_l, cid_r)) sg Nothing        (Just inner_r) = 
connSG (cid_super, (cid_l, cid_r)) sg (Just inner_l) (Just inner_r) = 

combine :: StructGraph -> StructGraph -> StructGraph
combine (MkSG n e snk src) (MkSG n' e' snk' src')
    = MkSG (n ++ n') (e ++ e') (snk ++ snk') (src ++ src')


isOuterEdge :: Connection -> Bool
isOuterEdge (Nothing, _) = True
isOuterEdge otherwise    = False


collectCompIDs :: StructGraph -> [CompID]
collectCompIDs sg = if isNothing sg'
                        then cid : []
                        else cid : (collectCompIDs $ fromJust sg')
    where cid = compID sg 
          sg' = node sg



newCompID :: StructGraph -> CompID
newCompID sg = nextID compIDs
    where compIDs = compID sg : (next $ node sg)
          next Nothing        = []
          next (Just innerSG) = compID innerSG : next (node innerSG)
          
    -- where compIDs = map compID $ node sg

nextID :: [CompID] -> CompID
nextID []    = 0
nextID [cid] = cid + 1
nextID cids  = nextID [foldl max 0 cids]


drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
