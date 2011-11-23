> {-# LANGUAGE Arrows,
>              OverlappingInstances, 
>              UndecidableInstances,
>              IncoherentInstances,
>              NoMonomorphismRestriction,
>              MultiParamTypeClasses,
>              FlexibleInstances,
>              RebindableSyntax #-}

> module GraphTraversal.Traversal 
> where

> import Prelude hiding (id, (.))

> import Control.Category

> import GraphTraversal.Core
> import GraphTraversal.Auxillary
> import GraphTraversal.Graph

> infixr 3 ***
> infixr 3 &&&


First thing to do is to define a new data type, which is called TraversalArrow.

> newtype TraversalArrow a b c = TR (a (b, Circuit) (c, Circuit))


Bevor the TraversalArrow becomes an instance of Arrow, it has to be an instance
of Category.

> instance (Arrow a) => Category (TraversalArrow a) where
>     id              = TR id
>     (TR f) . (TR g) = TR $ proc (x, sg) -> do
>                               (x_g, sg_g) <- g -< (x, sg)
>                               (x_f, sg_f) <- f -< (x_g, sg   `connect` sg_g)
>                               returnA          -< (x_f, sg_g `connect` sg_f)

We also need a definition of what an Arrow is, so here is the arrow-class.
Note that this class is slightly different from the vanilla-arrow-class, as there 
is a type constraint to the `arr`-function 

> class (Category a) => Arrow a where
>   arr    :: (ShowType b c) => (b -> c) -> a b c
>   first  :: a b c -> a (b, d) (c, d)
>   second :: a b c -> a (d, b) (d, c)
>   second f = arr swap >>> first f >>> arr swap
>            where swap :: (b, c) -> (c, b)
>                  swap   ~(x, y)  = (y, x)
>   (***)  :: a b c -> a b' c' -> a (b, b') (c, c')
>   f *** g = first f >>> second g
>   (&&&)  :: a b c -> a b c'  -> a b (c, c')
>   f &&& g = arr (\b -> (b, b)) >>> f *** g

> returnA :: (Arrow a) => a b b
> returnA = arr id



This class and it's instances define the different type-variants that are possible

> class ShowType b c where
>   showType :: (b -> c) -> Circuit

> instance ShowType (b, c) (c, b) where
>   showType _ = emptyGraph { name = "|b,c>c,b|" 
>                           , sinks = mkPins 2
>                           , sources = mkPins 2
>                           }
  
> instance ShowType b (b, b) where
>   showType _ = emptyGraph { name = "|b>b,b|"
>                           , sinks = mkPins 1
>                           , sources = mkPins 2
>                           }
  
> instance ShowType (b, b) b where
>   showType _ = emptyGraph { name = "|b,b>b|"
>                           , sinks = mkPins 2
>                           , sources = mkPins 1
>                           }
  
 instance ShowType (b, c) (b', c') where
   showType _ = emptyGraph { name = "b,c>b',c'"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }
  
instance ShowType b b where
  showType _ = emptyGraph { name = "|b>b|"
                          , sinks = mkPins 1
                          , sources = mkPins 1
                          }
  
 instance ShowType (b -> (c, d)) where
   showType _ = emptyGraph { name = "b>c,d"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }

 instance ShowType ((b, c) -> d) where
   showType _ = emptyGraph { name = "b,c>d"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }
  
> instance ShowType b c where
>   showType _ = emptyGraph { name = "|b>c|"
>                           , sinks = mkPins 1
>                           , sources = mkPins 1
>                           }


> movebrc :: ((a, b), c) -> (a, (b, c))
> movebrc ~(~(x, y), sg) = (x, (y, sg))

> backbrc :: (a, (b, c)) -> ((a, b), c)
> backbrc ~(x, ~(y, sg)) = ((x, y), sg)

> swapsnd :: ((a, b), c) -> ((a, c), b)
> swapsnd ~(~(x, y), sg) = ((x, sg), y)



> instance (Arrow a) => Arrow (TraversalArrow a) where
>     arr   f       
>       = TR $ arr (\(x, sg) -> (f x, showType f))
>
>     first  (TR f) 
>       = TR $   arr swapsnd 
>            >>> first f 
>            >>> arr swapsnd 
>            >>> second (arr (flip combine idGraph))
>
>     second (TR f) 
>       = TR $   arr movebrc
>            >>> second f 
>            >>> arr backbrc
>            >>> second (arr (combine idGraph))
>
>     (TR f) &&& (TR g) 
>       = TR $   dup
>            >>> first  f
>            >>> second g
>            >>> dup
>            >>> first  (arr (\x -> (,)        (fst.fst$x) (fst.snd$x)))
>            >>> second (arr (\s -> dupCombine (snd.fst$s) (snd.snd$s)))
>       where dup = arr (\x -> (x, x))
>
>     (TR f) *** (TR g) 
>       = TR $   dup
>            >>> first  (arr (\x -> (,) (fst.fst$x) (snd$x)))
>            >>> second (arr (\x -> (,) (snd.fst$x) (snd$x)))
>            >>> first  f
>            >>> second g
>            >>> dup
>            >>> first  (arr (\x -> (,)     (fst.fst$x) (fst.snd$x)))
>            >>> second (arr (\s -> combine (snd.fst$s) (snd.snd$s)))
>       where dup = arr (\x -> (x, x))


We need here two versions of ArrowChoice,
one, that processes every path and this one, that processes 
only a specific path. (btw, the combine-function here is still not 
the correct combinator for the two StructGraph's)

instance (ArrowChoice a, Typeable a) => ArrowChoice (TraversalArrow a) where
    left (TR f) = TR $ arr distr >>> left f >>> arr undistr
        where distr   (Left  x, sg)   = Left  (x, sg) 
              distr   (Right x, sg)   = Right (x, sg) 
              undistr (Left  (x, sg)) = (Left  x, sg `combine` leftGraph)
              undistr (Right (x, sg)) = (Right x, sg `combine` rightGraph)

> runTraversal :: (Arrow a) => TraversalArrow a b c -> a (b, Circuit) (c, Circuit)
> runTraversal (TR f) = f


Here the notation for an empty graph is used, to start the computation ... 

> runTraversal_ f x = runTraversal f (x, NoSG)
> rt = runTraversal_

rt aAdd (1,1) 

(2, 
  ... ... 
)


A usual process in the hardware development is the synthetisation of the actual 
hardware. Beside the syntethesis the simulation is also an important process while
developing hardware. Both functionalities are defined in the following:

synthesize :: (Arrow a) => TraversalArrow a b c -> b -> Circuit

> synthesize f x = flatten $ snd $ runTraversal f (x, NoSG)

simulate :: (Arrow a) => TraversalArrow a b c -> b -> c

> simulate f x = fst $ runTraversal f (x, NoSG)


Ist synthesize mit unit () möglich? 
Keine zufälligen daten ... 
Frage nach simulate / synthesize => right ... 
_kritische_pfad_analyse_ / ... 


> instance Arrow (->) where
>   arr f    = f
>   first  f = (\(x, y) -> (f x, y)) -- f *** id -- this results in an endless loop ...


> class Arrow a => (ArrowTraversal a) where
>   fetch :: a e  Circuit
>   store :: a Circuit ()

> x :: Arrow a => TraversalArrow a b b 
> x = arr id

> instance Arrow a => (ArrowTraversal (TraversalArrow a)) where
>   fetch = TR $ arr (\(_, sg) -> (sg, sg))
>   store = TR $ arr (\(sg, _) -> ((), sg))


> insert :: b -> (a, b) -> (a, b)
> insert sg ~(x, _) = (x, sg)

> insEmpty = insert emptyGraph { name = "eeeempty", sinks = mkPins 1, sources = mkPins 3 }

> augment :: (Arrow a) => Circuit -> TraversalArrow a b c -> TraversalArrow a b c
> augment sg (TR f) = TR $ f >>> arr (insert sg)

> y :: Arrow a => TraversalArrow a b b
> y = augment emptyGraph  x
