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
connect g_l g_r = MkSG { getNodes   = [ MkSGNode { subGraph   = newSG
                                                 , sinkPins   = map (\(_, pid) -> pid) sinks
                                                 , sourcePins = map (\(_, pid) -> pid) sources
                                                 , compID     = newCompID newSG 
                                                 }
                                      ]
                       , getEdges   = []
                       , getSinks   = sinks
                       , getSources = sources
                       }
    where left    = getSources g_l
          right   = getSinks   g_r
          nodes   = getNodes g_l ++ getNodes g_r
          intern  = zipWith MkEdge left right ++ getEdges g_l ++ getEdges g_r
          sinks   = getSinks g_l
          sources = getSources g_r
          cID     = newCompID newSG 
          newSG   = MkSG { getNodes   = nodes
                         , getEdges   = intern
                         , getSinks   = sinks
                         , getSources = sources
                         }
          

combine :: StructGraph -> StructGraph -> StructGraph
combine (MkSG n e snk src) (MkSG n' e' snk' src')
    = MkSG (n ++ n') (e ++ e') (snk ++ snk') (src ++ src')


newCompID :: StructGraph -> CompID
newCompID sg = nextID compIDs
    where compIDs    = map compID $ getNodes sg
          nextID []    = 0
          nextID [cid] = cid + 1
          nextID cids  = nextID [foldl max 0 cids]


drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
