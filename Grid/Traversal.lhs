> {-# LANGUAGE Arrows,
>              OverlappingInstances, 
>              UndecidableInstances,
>              IncoherentInstances,
>              NoMonomorphismRestriction,
>              MultiParamTypeClasses,
>              FlexibleInstances,
>              RebindableSyntax #-}

> module Grid.Traversal 
> where

> import Prelude hiding (id, (.))
> import qualified Prelude as Pr

> import Control.Category 
> -- import Control.Monad.Fix (mfix)

> import Grid.Core
> import Grid.Auxillary
> import Grid.Graph
> import Grid.Workers (flatten)

> infixr 3 ***
> infixr 3 &&&


First thing to do is to define a new data type, which is called Grid (TraversalArrow)

> newtype Grid a b c = GR (a (b, Circuit) (c, Circuit))


Bevor the Grid (TraversalArrow) becomes an instance of Arrow, it has to be an instance
of Category.

> instance (Arrow a) => Category (Grid a) where
>     id              = GR id
>     (GR f) . (GR g) = GR $ proc (x, sg) -> do
>                               (x_g, sg_g) <- g -< (x, sg)
>                               (x_f, sg_f) <- f -< (x_g, sg   `connect` sg_g)
>                               returnA          -< (x_f, sg_g `connect` sg_f)

TODO:
TODO: better would be a solution similar to the following. This is because one could not 
assume, that an a is alway an Arrow before we make it a Category ... Better would also be
to make (b, Circuit) `a` (c, Circuit) an instance of Category, so we could use this
attribute in the definition like ...
     id              = GR $ \(x, sg) -> (Pr.id $ x, sg)
     (GR f) . (GR g) = GR $ \(x, sg) ->
                               let (x_g, sg_g) = g (x, sg)
                                   (x_f, sg_f) = f (x_g, sg `connect` sg_g)
                               in  (x_f, sg_g `connect` sg_f)

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
>   showType _ = emptyCircuit { label = "|b,c>c,b|" 
>                           , sinks = mkPins 2
>                           , sources = mkPins 2
>                           }
  
> instance ShowType b (b, b) where
>   showType _ = emptyCircuit { label = "|b>b,b|"
>                           , sinks = mkPins 1
>                           , sources = mkPins 2
>                           }
  
> instance ShowType (b, b) b where
>   showType _ = emptyCircuit { label = "|b,b>b|"
>                           , sinks = mkPins 2
>                           , sources = mkPins 1
>                           }
  
 instance ShowType (b, c) (b', c') where
   showType _ = emptyCircuit { label = "b,c>b',c'"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }
  
instance ShowType b b where
  showType _ = emptyCircuit { label = "|b>b|"
                          , sinks = mkPins 1
                          , sources = mkPins 1
                          }
  
 instance ShowType (b -> (c, d)) where
   showType _ = emptyCircuit { label = "b>c,d"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }

 instance ShowType ((b, c) -> d) where
   showType _ = emptyCircuit { label = "b,c>d"
                           , sinks = mkPins 1
                           , sources = mkPins 1
                           }
  
> instance ShowType b c where
>   showType _ = emptyCircuit { label = "|b>c|"
>                           , sinks = mkPins 1
>                           , sources = mkPins 1
>                           }


> movebrc :: ((a, b), c) -> (a, (b, c))
> movebrc ~(~(x, y), sg) = (x, (y, sg))

> backbrc :: (a, (b, c)) -> ((a, b), c)
> backbrc ~(x, ~(y, sg)) = ((x, y), sg)

> swapsnd :: ((a, b), c) -> ((a, c), b)
> swapsnd ~(~(x, y), sg) = ((x, sg), y)



> instance (Arrow a) => Arrow (Grid a) where
>     arr   f       
>       = GR $ arr (\(x, sg) -> (f x, showType f))
>
>     first  (GR f) 
>       = GR $   arr swapsnd 
>            >>> first f 
>            >>> arr swapsnd 
>            >>> second (arr (flip combine idCircuit))
>
>     second (GR f) 
>       = GR $   arr movebrc
>            >>> second f 
>            >>> arr backbrc
>            >>> second (arr (combine idCircuit))
>
>     (GR f) &&& (GR g) 
>       = GR $   dup
>            >>> first  f
>            >>> second g
>            >>> dup
>            >>> first  (arr (\x -> (,)        (fst.fst$x) (fst.snd$x)))
>            >>> second (arr (\s -> dupCombine (snd.fst$s) (snd.snd$s)))
>       where dup = arr (\x -> (x, x))
>
>     (GR f) *** (GR g) 
>       = GR $   dup
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

instance (ArrowChoice a, Typeable a) => ArrowChoice (Grid a) where
    left (GR f) = GR $ arr distr >>> left f >>> arr undistr
        where distr   (Left  x, sg)   = Left  (x, sg) 
              distr   (Right x, sg)   = Right (x, sg) 
              undistr (Left  (x, sg)) = (Left  x, sg `combine` leftCircuit)
              undistr (Right (x, sg)) = (Right x, sg `combine` rightCircuit)


For getting loops done and measurements in the circuit, there are streaming-functions
needed. Therefore a stream-function datatype is defined with name `Stream` and with 
typeconstructur `SF`. This Datatype consists of a function, that takes a list of a's to 
a list of b's.

> newtype Stream b c = SF { runStream :: ([b] -> [c]) }


The next step is to make this stream a `Category` and after that make 
it an Arrow.

> instance Category Stream where
>     id              = SF (id)
>     (SF f) . (SF g) = SF (f . g) 

> instance Arrow Stream where
>     arr          = SF . map 
>     first (SF f) = SF $ (uncurry zip) . (\(bs, cs) -> (f bs, cs)) . unzip 


--- 
Not sure if the following still makes any sense 

> class Arrow a => ArrowLoop a where
>     loop :: a (b,d) (c,d) -> a b c

> instance (ArrowLoop a) => ArrowLoop (Grid a) where
>     loop (GR f) = GR (loop (arr swapsnd >>> f >>> arr swapsnd))


--- 

However, with a stream function a loop is alway's possible, so the next
step is to make Stream a member of ArrowLoop

> instance ArrowLoop Stream where
>     loop (SF f) = SF $ \bs -> 
>         let (cs, ds) = unzip (f (zip bs (stream ds))) in cs
>      where stream ~(x:xs) = x:stream xs


And with the ArrowLoop instance, it is straigt forward, to lift the Stream 
into the ArrowCircuit instance. This is the one, that holds a delay component
and first of all, the class definition is given here.

> class ArrowLoop a => ArrowCircuit a where
>     delay :: b -> a b b

> instance ArrowCircuit Stream where
>     delay x = SF (x:)




> runGrid :: (Arrow a) => Grid a b c -> a (b, Circuit) (c, Circuit)
> runGrid (GR f) = f


Here the notation for an empty graph is used, to start the computation ... 

> runGrid_ f x = runGrid f (x, NoSG)
> rt = runGrid_

rt aAdd (1,1) 

(2, 
  ... ... 
)


A usual process in the hardware development is the synthetisation of the actual 
hardware. Beside the syntethesis the simulation is also an important process while
developing hardware. Both functionalities are defined in the following:

synthesize :: (Arrow a) => Grid a b c -> b -> Circuit

> synthesize f x = flatten $ snd $ runGrid f (x, NoSG)

simulate :: (Arrow a) => Grid a b c -> b -> c

> simulate f x = fst $ runGrid f (x, NoSG)


Ist synthesize mit unit () möglich? 
Keine zufälligen daten ... 
Frage nach simulate / synthesize => right ... 
_kritische_pfad_analyse_ / ... 


> instance Arrow (->) where
>   arr f    = f
>   first  f = (\(x, y) -> (f x, y)) -- f *** id -- this results in an endless loop ...


> class Arrow a => (ArrowGrid a) where
>   fetch :: a e  Circuit
>   store :: a Circuit ()

> x :: Arrow a => Grid a b b 
> x = arr id

> instance Arrow a => (ArrowGrid (Grid a)) where
>   fetch = GR $ arr (\(_, sg) -> (sg, sg))
>   store = GR $ arr (\(sg, _) -> ((), sg))


> insert :: b -> (a, b) -> (a, b)
> insert sg ~(x, _) = (x, sg)

> insEmpty = insert emptyCircuit { label = "eeeempty", sinks = mkPins 1, sources = mkPins 3 }

> augment :: (Arrow a) => Circuit -> Grid a b c -> Grid a b c
> augment sg (GR f) = GR $ f >>> arr (insert sg)

> y :: Arrow a => Grid a b b
> y = augment emptyCircuit  x
