module GraphTraversal.Auxillary 
    ( connect
    , combine
    , newCompID
    , drop_first
    , drop_second
    )
where

import Control.Arrow

import GraphTraversal.Core

connect :: StructGraph -> StructGraph -> StructGraph
connect left right = MkSG { name    = (name left') ++ ">>>" ++ (name right')
                          , compID  = nextID $ (allCompIDs left') ++ (allCompIDs right')
                          , nodes   = left' : right' : []
                          , edges   = rewire (sources left', compID left') (sinks right', compID right')
                          , sinks   = sinks left'
                          , sources = sources right'
                          }
    where [left',right'] = fst.unifyCompIDs $ ([left, right], 0)


combine :: StructGraph -> StructGraph -> StructGraph
combine left right = MkSG { name    = (name left') ++ "+++" ++ (name right') 
                          , compID  = nextID $ (allCompIDs left') ++ (allCompIDs right')
                          , nodes   = left' : right' : []
                          , edges   = (edges left') ++ (edges right') 
                          , sinks   = (sinks left') ++ (sinks right')
                          , sources = (sources left') ++ (sources right')
                          }
    where [left', right'] = fst.unifyCompIDs $ ([left, right], 0)
    


allCompIDs :: StructGraph -> [CompID]
allCompIDs sg 
    = if (length next_sg == 0) then cid : []
                               else cid : (concat $ map allCompIDs next_sg)
    where next_sg = nodes sg
          cid     = compID sg 


rewire :: ([SourceEdge], CompID) -> ([SinkEdge], CompID) -> [Edge]
rewire (srcs, cid_l) (snks, cid_r) 
    = zipWith (\src snk -> MkEdge (Just $ cid_l, src)
                                  (Just $ cid_r, snk)) (map snd srcs) (map snd snks)


unifyCompID :: (StructGraph, CompID) -> (StructGraph, CompID)
unifyCompID (sg, cid) 
    = ( sg { compID = cid
           , nodes  = sub_sg' 
           }
      , cid_next
      )
    where (sub_sg', cid_next) = unifyCompIDs (nodes sg, (cid+1))


unifyCompIDs :: ([StructGraph], CompID) -> ([StructGraph], CompID)
unifyCompIDs ([],     cid) = ([], cid)
unifyCompIDs (sg:sgs, cid) = (sg' : sgs', cid'')
    where (sg',  cid')  = unifyCompID  (sg, cid)
          (sgs', cid'') = unifyCompIDs (sgs, cid')


newCompID :: StructGraph -> CompID
newCompID sg = nextID compIDs
    where compIDs = compID sg : (next $ nodes sg)
          next []        = []
          next [innerSG] = compID innerSG : next (nodes innerSG)
          
    -- where compIDs = map compID $ node sg

nextID :: [CompID] -> CompID
nextID []    = 0
nextID [cid] = cid + 1
nextID cids  = nextID [foldl max 0 cids]


drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
