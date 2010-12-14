module Traversal.Auxillary 
    ( with_predecessor
    , with_predecessors
    , drop_first
    , drop_second
    )
where

import Control.Arrow

import Data.Either ( lefts
                   , rights
                   )

import Traversal.Structure

with_predecessor :: Structure -> Structure -> Structure
with_predecessor object@(Annotate { predecessor = Left preds }) precursor 
    | null preds = object { predecessor = Left  [precursor] }
    | otherwise  = object { predecessor = Left $ map (flip with_predecessor precursor) preds }
with_predecessor object@(Annotate { predecessor = Right preds }) precursor 
    | null preds = object { predecessor = Right [precursor] }
    | otherwise  = object { predecessor = Right $ map (flip with_predecessor precursor) preds }

with_predecessors :: Structure -> [Structure] -> Structure 
with_predecessors object@(Annotate { predecessor = Left  preds }) precursors 
    = object { predecessor = Left  $ preds ++ precursors } 
with_predecessors object@(Annotate { predecessor = Right preds }) precursors 
    = object { predecessor = Right $ preds ++ precursors } 

drop_first  :: (Arrow a) => a (b, b') b'
drop_first  =  arr (\(x, y) -> y)

drop_second :: (Arrow a) => a (b, b') b
drop_second =  arr (\(x, y) -> x)
