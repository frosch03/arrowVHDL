module GraphTraversal.Auxillary 
    ( connect
    , combine
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
connect g_l g_r = MkSG { getNodes   = nodes
                       , getEdges   = intern
                       , getSinks   = sinks
                       , getSources = sources
                       }
    where left    = getSources g_l
          right   = getSinks   g_r
          nodes   = getNodes g_l ++ getNodes g_r
          intern  = zipWith MkEdge left right ++ getEdges g_l ++ getEdges g_r
          sinks   = getSinks g_l
          sources = getSources g_r
          

combine :: StructGraph -> StructGraph -> StructGraph
combine (MkSG n e snk src) (MkSG n' e' snk' src')
    = MkSG (n ++ n') (e ++ e') (snk ++ snk') (src ++ src')


drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
