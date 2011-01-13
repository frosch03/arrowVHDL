{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fglasgow-exts #-} 
module GraphTraversal.Traversal 
    ( runTraversal
    , runTraversal_
    , TraversalArrow (..)
    , Augment
    , augment
    , emptyGraph
    )
where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer

import GraphTraversal.Core
import GraphTraversal.Auxillary

emptyGraph :: StructGraph
emptyGraph = MkSG [] [] [] []

newtype TraversalArrow a b c = TR (a (b, StructGraph) (c, StructGraph))

instance (Category a, Arrow a) => Category (TraversalArrow a) where
    id              = TR id
    (TR f) . (TR g) = TR $ proc (x, sg) -> do
                            (x', sg_g) <- g -< (x,  sg)
                            (y,  sg_f) <- f -< (x', sg_g `connect` sg  )
                            returnA         -< (y,  sg_f `connect` sg_g)


instance (Arrow a) => Arrow (TraversalArrow a) where
    arr f        = TR (arr (\(x, _) -> (f x, emptyGraph)))
    first (TR f) = TR (arr swapsnd >>> first f >>> arr swapsnd)
     where swapsnd ((x, y), sg) = ((x, sg), y)
    (TR f) *** (TR g) = TR $ proc ((x, y), sg) -> do 
                            (x', sg_f) <- f -< (x,   sg)
                            (y', sg_g) <- g -< (y,   sg)
                            returnA         -< ((x', y'), sg_f `combine` sg_g)
                          


instance (Arrow a) => ArrowTransformer (TraversalArrow) a where
    lift f = TR (first f)

runTraversal :: (Arrow a) => TraversalArrow a b c -> a (b, StructGraph) (c, StructGraph)
runTraversal (TR f) = f

runTraversal_ f x = runTraversal f (x, emptyGraph)


-- class (Arrow a) => Augment t1 t2 a b c where
--     augment :: (Arrow a) => t1 -> t2 -> TraversalArrow a b c 
-- 
-- instance (Arrow a) => Augment (a b c) (a () StructGraph) a b c where
--     augment aA aSG 
--         = TR $ proc (x, sg) -> do
--             sg' <- aSG -< ()
--             x'  <- aA  -< x
--             returnA    -< (x', sg')
-- 
-- instance (Arrow a) => Augment (a b c) (StructGraph) a b c where
--     augment aA sg 
--         = augment aA 
--                 ((arr (\_ -> sg))   :: Arrow a => a () StructGraph)
-- 
-- instance (Arrow a) => Augment (b -> c) (StructGraph) a b c where
--     augment f sg 
--         = augment ((arr f)          :: Arrow a => a b c) 
--                   ((arr (\_ -> sg)) :: Arrow a => a () StructGraph)
-- 
-- instance (Arrow a) => Augment (TraversalArrow a b c) (StructGraph) a b c where
--     augment (TR f) sg 
--         = TR $ proc (x, s) -> do
--             (x', _) <- f -< (x,  s) 
--             returnA      -< (x', sg)
