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

import GraphTraversal.Core

connect :: StructGraph -> StructGraph -> StructGraph
connect g_l g_r = MkSG { getName    = (getName g_l ++ ">>>" ++ getName g_r)
                       , getCompID  = nextID [cid_l, cid_r]
                       , getNode    = connSG (getNode g_l) (getNode g_r)
                       , getEdges   = newedges ++ getEdges g_l ++ getEdges g_r
                       , getSinks   = getSinks g_l
                       , getSources = getSources g_r
                       }
    where cid_l   = getCompID g_l
          cid_r   = getCompID g_r

          srcs_l  = getSources g_l
          snks_r  = getSinks g_r

          connSG :: Maybe StructGraph -> Maybe StructGraph -> Maybe StructGraph
          connSG Nothing Nothing = Nothing

          newedges = zipWith (\src snk -> MkEdge (Just cid_l, src) 
                                                 (Just cid_r, snk)) srcs_l snks_r
--     where name    = (getName g_l ++ ">>>" ++ getName g_r)
--           compID  = nextID [cid_l, cid_r] 
--           node    = connSG (getNode g_l) (getNode g_r)
--           edges   = newedges ++ getEdges g_l ++ getEdges g_r
--           sinks   = getSinks g_l
--           sources = getSources g_r
-- 






--     where left    = getSources g_l
--           right   = getSinks   g_r
--           nodes   = getNodes g_l ++ getNodes g_r
--           intern  = zipWith MkEdge left right ++ getEdges g_l ++ getEdges g_r
--           sinks   = getSinks g_l
--           sources = getSources g_r
--           cID     = newCompID newSG 
--           newSG   = MkSG { getNodes   = nodes
--                          , getEdges   = intern
--                          , getSinks   = sinks
--                          , getSources = sources
--                          }
          

combine :: StructGraph -> StructGraph -> StructGraph
combine (MkSG n e snk src) (MkSG n' e' snk' src')
    = MkSG (n ++ n') (e ++ e') (snk ++ snk') (src ++ src')


newCompID :: StructGraph -> CompID
newCompID sg = nextID compIDs
    where compIDs = getCompID sg : (next $ getNode sg)
          next Nothing        = []
          next (Just innerSG) = getCompID innerSG : next (getNode innerSG)
          
    -- where compIDs = map compID $ getNode sg

nextID :: [CompID] -> CompID
nextID []    = 0
nextID [cid] = cid + 1
nextID cids  = nextID [foldl max 0 cids]


drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
