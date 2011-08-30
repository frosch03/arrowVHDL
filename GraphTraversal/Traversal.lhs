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

> newtype TraversalArrow a b c = TR (a (b, StructGraph) (c, StructGraph))


Bevor the TraversalArrow becomes an instance of Arrow, it has to be an instance
of Category.

> instance (Arrow a) => Category (TraversalArrow a) where
>     id              = TR id
>     (TR f) . (TR g) = TR $ proc (x, sg) -> do
>                               (x_g, sg_g) <- g -< (x, sg)
>                               (x_f, sg_f) <- f -< (x_g, sg   `connect` sg_g)
>                               returnA          -< (x_f, sg_g `connect` sg_f)
>--     (TR f) . (TR g) = TR $   dup 
>--                          >>> g *** arr snd
>--                          >>> first flip
>--                          >>> arr swapsnd
>--                          >>> first (   flip
>--                                    >>> arr (uncurry connect)
>--                                    )
>--                          >>> flip
>--
>--                          >>> dup
>--                          >>> f *** arr snd
>--                          >>> first flip
>--                          >>> arr swapsnd
>--                          >>> first (   flip
>--                                    >>> arr (uncurry connect)
>--                                    )
>--                          >>> flip
>--                      where flip = arr (\(x, y) -> (y, x))
>--                            dup  = arr (\x -> (x, x))
>--   (TR f) . (TR g) = TR (f . g)

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
>   showType :: (b -> c) -> StructGraph

> instance ShowType (b, c) (c, b) where
>   showType _ = emptyGraph { name = "|b,c>c,b|" 
>                           , sinks = mkPins 1
>                           , sources = mkPins 1
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
  
> instance ShowType b b where
>   showType _ = emptyGraph { name = "|b>b|"
>                           , sinks = mkPins 1
>                           , sources = mkPins 1
>                           }
  
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

> runTraversal :: (Arrow a) => TraversalArrow a b c -> a (b, StructGraph) (c, StructGraph)
> runTraversal (TR f) = f


Here the notation for an empty graph is used, to start the computation ... 

> runTraversal_ f x = runTraversal f (x, NoSG)
> rt = runTraversal_


> instance Arrow (->) where
>   arr f    = f
>   first  f = (\(x, y) -> (f x, y)) -- f *** id -- this results in an endless loop ...


> class Arrow a => (ArrowTraversal a) where
>   fetch :: a e  StructGraph
>   store :: a StructGraph ()

> x :: Arrow a => TraversalArrow a b b 
> x = arr id

> instance Arrow a => (ArrowTraversal (TraversalArrow a)) where
>   fetch = TR $ arr (\(_, sg) -> (sg, sg))
>   store = TR $ arr (\(sg, _) -> ((), sg))


> insert :: b -> (a, b) -> (a, b)
> insert sg ~(x, _) = (x, sg)

> insEmpty = insert emptyGraph { name = "eeeempty", sinks = mkPins 1, sources = mkPins 3 }

> augment :: (Arrow a) => StructGraph -> TraversalArrow a b c -> TraversalArrow a b c
> augment sg (TR f) = TR $ f >>> arr (insert sg)

> y :: Arrow a => TraversalArrow a b b
> y = augment emptyGraph  x



> -- augment_aA_aSG :: (Arrow a) => (a b c) -> (a () StructGraph) -> TraversalArrow a b c
> -- augment_aA_aSG aA aSG 
> --     = TR $ proc (x, sg) -> do
> --         sg' <- aSG -< ()
> --         x'  <- aA  -< x
> --         returnA    -< (x', sg')

> -- augment_aA_SG :: (Arrow a) => (a b c) -> (StructGraph) -> TraversalArrow a b c
> -- augment_aA_SG aA sg 
> --     = augment_aA_aSG aA (arr (\_ -> sg))

> -- augment_f_SG :: (Arrow a) => (b -> c) -> (StructGraph) -> TraversalArrow a b c
> -- augment_f_SG f sg 
> --     = augment_aA_aSG (arr f) (arr (\_ -> sg))

> -- augment_aTA_SG :: (Arrow a) => (TraversalArrow a b c) -> (StructGraph) -> TraversalArrow a b c
> -- augment_aTA_SG (TR f) sg 
> --     = TR $ proc (x, s) -> do
> --         (x', _) <- f -< (x,  s) 
> --         returnA      -< (x', sg)




This is the advanced overloading solution ...  

class B b where 
  bV :: b -> String
class C c where
  cV :: c -> String
class BC b c where
  bcV :: (b -> c) -> String
class N_BC a b c where
  n_bcV :: a -> (b -> c) -> String

class BPrint  b   where bprint  :: b  -> String
class CPrint  c   where cprint  :: c  -> String
class BCPrint b c where bcprint :: (b -> c) -> String
class N_BCPrint a b c where n_bcprint :: a -> (b -> c) -> String


class BPrint' flag b where
  bprint' :: flag -> b -> String
class CPrint' flag c where
  cprint' :: flag -> c -> String
class BCPrint' flag b c where
  bcprint' :: flag -> (b -> c) -> String
class N_BCPrint' flag a b c where
  n_bcprint' :: flag -> a -> (b -> c) -> String

instance (BPred b flag, BPrint' flag b) => BPrint b where
  bprint = bprint' (undefined::flag)
instance (CPred c flag, CPrint' flag c) => CPrint c where
  cprint = cprint' (undefined::flag)
instance (BCPred b c flag, BCPrint' flag b c) => BCPrint b c where
  bcprint = bcprint' (undefined::flag)
instance (N_BCPred a b c flag, N_BCPrint' flag a b c) => N_BCPrint a b c where
  n_bcprint = n_bcprint' (undefined::flag) (undefined::a)

class BPred b flag |  b->flag where {}
class CPred c flag |  c->flag where {}
class BCPred b c flag | b->c, c->flag where {}
class N_BCPred a b c flag | b->c, c->flag where { dummy :: a b c }

instance TypeCast flag HFalse => BPred a flag
instance TypeCast flag HFalse => CPred a flag
instance TypeCast flag HFalse => BCPred b c flag
instance TypeCast flag HFalse => N_BCPred a b c flag


instance BPred (a,a) HTrue 
instance BPred (a,b) HTrue
instance BPred a HTrue
instance CPred (a,a) HTrue 
instance CPred (a,b) HTrue
instance CPred a HTrue
instance BCPred (b,b)  (c,c)  HTrue
instance BCPred (b,b') (c,c)  HTrue
instance BCPred (b,b)  (c,c') HTrue
instance BCPred (b)    (c,c)  HTrue
instance BCPred (b)    (c,c') HTrue
instance BCPred (b,b)  (c)    HTrue
instance BCPred (b,b') (c)    HTrue
instance BCPred (b)    (c)    HTrue
instance N_BCPred a (b,b)  (c,c)  HTrue
instance N_BCPred a (b,b') (c,c)  HTrue
instance N_BCPred a (b,b)  (c,c') HTrue
instance N_BCPred a (b)    (c,c)  HTrue
instance N_BCPred a (b)    (c,c') HTrue
instance N_BCPred a (b,b)  (c)    HTrue
instance N_BCPred a (b,b') (c)    HTrue
instance N_BCPred a (b)    (c)    HTrue


instance B (a,a) where bV _ = "(a,a)"
instance B (a,b) where bV _ = "(a,b)" 
instance B a where bV _ = "a" 
instance C (a,a) where cV _ = "(a,a)"
instance C (a,b) where cV _ = "(a,b)" 
instance C a where cV _ = "a" 
instance BC (b,b) (c,c) where bcV _   = "(b,b) -> (c,c)"
instance BC (b,b') (c,c) where bcV _  = "(b,b') -> (c,c)"
instance BC (b,b)  (c,c') where bcV _ = "(b,b) -> (c,c')"
instance BC (b)    (c,c) where bcV _  = "b -> (c,c)"
instance BC (b)    (c,c') where bcV _ = "b -> (c,c')"
instance BC (b,b)  (c) where bcV _    = "(b,b) -> c"
instance BC (b,b') (c) where bcV _    = "(b,b') -> c"
instance BC (b)    (c) where bcV _    = "b -> c"
instance N_BC a (b,b) (c,c) where n_bcV _ _   = "(b,b) -> (c,c)"
instance N_BC a (b,b') (c,c) where n_bcV _ _  = "(b,b') -> (c,c)"
instance N_BC a (b,b)  (c,c') where n_bcV _ _ = "(b,b) -> (c,c')"
instance N_BC a (b)    (c,c) where n_bcV _ _  = "b -> (c,c)"
instance N_BC a (b)    (c,c') where n_bcV _ _ = "b -> (c,c')"
instance N_BC a (b,b)  (c) where n_bcV _ _    = "(b,b) -> c"
instance N_BC a (b,b') (c) where n_bcV _ _    = "(b,b') -> c"
instance N_BC a (b)    (c) where n_bcV _ _    = "b -> c"

instance (B b) => BPrint' HTrue b where
  bprint' _ x = bV (undefined::b)
instance (C c) => CPrint' HTrue c where
  cprint' _ x = cV (undefined::c)
instance (BC b c) => BCPrint' HTrue b c where
  bcprint' _ f = bcV (undefined::(b -> c))
instance (N_BC a b c) => N_BCPrint' HTrue a b c where
  n_bcprint' _ _ f = n_bcV (undefined::a) (undefined::(b -> c))

instance BPrint' HFalse b where 
  bprint' _ x = "Not in B"
instance CPrint' HFalse c where 
  cprint' _ x = "Not in C"
instance BCPrint' HFalse b c where 
  bcprint' _ x = "Not in BC"
instance N_BCPrint' HFalse a b c where 
  n_bcprint' _ _ x = "Not in BC"


data HTrue
data HFalse

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x 
