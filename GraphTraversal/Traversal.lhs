> {-# LANGUAGE Arrows #-}
> {-# OPTIONS_GHC -fglasgow-exts #-} 
> module GraphTraversal.Traversal 
>     ( runTraversal
>     , runTraversal_
>     , rt
>     , TraversalArrow (..)
>     , augment_aA_aSG
>     , augment_aA_SG
>     , augment_f_SG
>     , augment_aTA_SG
>     , emptyGraph
>     )
> where

> import Prelude hiding (id, (.))

> import Control.Category
> import Control.Arrow
> import Control.Arrow.Transformer

> import Data.Either ( either )

> import GraphTraversal.Core
> import GraphTraversal.Auxillary

> emptyGraph :: StructGraph
> emptyGraph = MkSG { name    = ""
>                   , compID  = 0
>                   , nodes   = []
>                   , edges   = []
>                   , sinks   = []
>                   , sources = []
>                   }

> arrGraph = emptyGraph { name = " " }

> throughGraph = emptyGraph { name    = "(-)"
>                           , sinks   = []
>                           , sources = []
>                           }

> leftGraph = emptyGraph { name    = "(L)"
>                        , sinks   = []
>                        , sources = []
>                        }

> rightGraph = emptyGraph { name    = "(R)"
>                         , sinks   = []
>                         , sources = []
>                         }


> newtype TraversalArrow a b c = TR (a (b, StructGraph) (c, StructGraph))

> instance (Category a, Arrow a) => Category (TraversalArrow a) where
>     id              = TR id
>     (TR f) . (TR g) = TR $ proc (x, sg) -> do
>                             (x', sg_g) <- g -< (x,  sg)
>                             (y,  sg_f) <- f -< (x', sg   `connect` sg_g)
>                             returnA         -< (y,  sg_g `connect` sg_f)



> instance (Arrow a) => Arrow (TraversalArrow a) where
>     arr f        = TR (arr (\(x, _) -> (f x, arrGraph)))
>     first (TR f) = TR $ proc ((x, y), sg) -> do
>                             (x', sg_f) <- f -< (x, sg)
>                             returnA         -< ((x', y), sg_f `combine` throughGraph)
>     second (TR g) = TR $ proc ((x, y), sg) -> do
>                              (y', sg_g) <- g -< (y, sg)
>                              returnA         -< ((x, y'), sg_g `combine` throughGraph)
>     (TR f) &&& (TR g) = TR $ proc (x, sg) -> do 
>                             (x', sg_f) <- f -< (x,   sg)
>                             (y', sg_g) <- g -< (x,   sg)
>                             returnA         -< ((x', y'), sg_f `combine` sg_g)
>     (TR f) *** (TR g) = TR $ proc ((x, y), sg) -> do 
>                             (x', sg_f) <- f -< (x,   sg)
>                             (y', sg_g) <- g -< (y,   sg)
>                             returnA         -< ((x', y'), sg_f `combine` sg_g)


We need here two versions of ArrowChoice,
one, that processes every path and this one, that processes 
only a specific path. (btw, the combine-function here is still not 
the correct combinator for the two StructGraph's)

> instance (ArrowChoice a) => ArrowChoice (TraversalArrow a) where
>     left (TR f) = TR $ arr distr >>> left f >>> arr undistr
>         where distr   (Left  x, sg)   = Left  (x, sg) 
>               distr   (Right x, sg)   = Right (x, sg) 
>               undistr (Left  (x, sg)) = (Left  x, sg `combine` leftGraph)
>               undistr (Right (x, sg)) = (Right x, sg `combine` rightGraph)

> runTraversal :: (Arrow a) => TraversalArrow a b c -> a (b, StructGraph) (c, StructGraph)
> runTraversal (TR f) = f

> runTraversal_ f x = runTraversal f (x, emptyGraph)
> rt = runTraversal_


> augment_aA_aSG :: (Arrow a) => (a b c) -> (a () StructGraph) -> TraversalArrow a b c
> augment_aA_aSG aA aSG 
>     = TR $ proc (x, sg) -> do
>         sg' <- aSG -< ()
>         x'  <- aA  -< x
>         returnA    -< (x', sg')

> augment_aA_SG :: (Arrow a) => (a b c) -> (StructGraph) -> TraversalArrow a b c
> augment_aA_SG aA sg 
>     = augment_aA_aSG aA (arr (\_ -> sg))

> augment_f_SG :: (Arrow a) => (b -> c) -> (StructGraph) -> TraversalArrow a b c
> augment_f_SG f sg 
>     = augment_aA_aSG (arr f) (arr (\_ -> sg))

> augment_aTA_SG :: (Arrow a) => (TraversalArrow a b c) -> (StructGraph) -> TraversalArrow a b c
> augment_aTA_SG (TR f) sg 
>     = TR $ proc (x, s) -> do
>         (x', _) <- f -< (x,  s) 
>         returnA      -< (x', sg)
