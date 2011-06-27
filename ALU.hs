{-# LANGUAGE Arrows, FlexibleContexts #-}
module ALU where

import Prelude hiding (and, or, not)
import Control.Arrow

import GraphTraversal.Traversal
import GraphTraversal.Show
import GraphTraversal.Core
import GraphTraversal.Auxillary

import GraphTraversal.Show.DOT as D


dup :: a -> (a, a)
dup x = (x, x)


tup22_121 :: ((a, b), (c, d)) -> (a, ((b, c), d))
tup22_121 = (\((x1, x2), (x3, x4)) -> (x1, ((x2, x3), x4)))

tup121_22 :: (a, ((b, c), d)) -> ((a, b), (c, d))
tup121_22 = (\(x1, ((x2, x3), x4)) -> ((x1, x2), (x3, x4)))

aDup :: (Arrow a) => TraversalArrow a (b) (b, b)
aDup 
     = augment_f_SG 
        dup
        emptyGraph { name    = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   } 

aId :: (Arrow a) => TraversalArrow a b b
aId 
    = augment_f_SG 
        (id) 
        emptyGraph { name    = "ID"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }

aId' :: (Arrow a) => TraversalArrow a b b
aId' 
    = augment_f_SG 
        (id) 
        emptyGraph { name    = "XX"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }

aFlip :: (Arrow a) => TraversalArrow a (b, c) (c, b)
aFlip
    = augment_f_SG 
        (\(x, y) -> (y, x))
        emptyGraph { name    = "FLIP"
                   , sinks   = mkPins 2
                   , sources = mkPins 2
                   }

aTup22_121 :: (Arrow a) => TraversalArrow a ((b, c), (d, e)) (b, ((c, d), e))
aTup22_121
    = augment_f_SG
        tup22_121
        emptyGraph { name    = "TUP22--121"
                   , sinks   = mkPins 2
                   , sources = mkPins 3
                   }

aTup121_22 :: (Arrow a) => TraversalArrow a (b, ((c, d), e)) ((b, c), (d, e))
aTup121_22
    = augment_f_SG
        tup121_22
        emptyGraph { name    = "TUP121--22"
                   , sinks   = mkPins 3
                   , sources = mkPins 2
                   }


and :: (Bool, Bool) -> Bool
and (True, True) = True
and otherwise    = False

or :: (Bool, Bool) -> Bool
or (False, False) = False
or otherwise      = True

not :: Bool -> Bool
not True  = False
not False = True

mux :: (Int, (Bool, Bool)) -> Bool
mux (0, (x, _)) = x
mux (1, (_, y)) = y


aAnd :: (Arrow a) => TraversalArrow a (Bool, Bool)        (Bool)
aAnd 
    = augment_f_SG
        and
        emptyGraph { name    = "AND"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }

aOr  :: (Arrow a) => TraversalArrow a (Bool, Bool)        (Bool)
aOr  
    = augment_f_SG
        or
        emptyGraph { name    = "OR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }

aNot :: (Arrow a) => TraversalArrow a (Bool)           (Bool)
aNot 
    = augment_f_SG
        not
        emptyGraph { name    = "NOT"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }

aMuxF :: (Arrow a) => TraversalArrow a (Int, (Bool, Bool)) (Bool)
aMuxF 
    = augment_f_SG
        mux
        emptyGraph { name    = "MUX"
                   , sinks   = mkPins 3
                   , sources = mkPins 1
                   }


aAlu :: (Arrow a) => TraversalArrow a (Int, (Bool, Bool)) (Bool)
aAlu 
    =
    (
        aId
    *** (   (   aDup 
            *** aDup
            )
        >>> aTup22_121
        >>> (   aId
            *** (   aFlip
                *** aId
                )
            )
        >>> aTup121_22
        >>> (   aOr
            *** aAnd
            )
        )
    )
    >>> aMuxF

aAlu' :: (Arrow a) => TraversalArrow a (Int, (Bool, Bool)) (Bool)
aAlu' 
    = ( proc (sw, (i0, i1)) -> do
            or_o0  <- aOr  -< (i0, i1)
            and_o0 <- aAnd -< (i0, i1)
            o0     <- aMuxF -< (sw, (or_o0, and_o0))
            returnA        -< o0
      )

aAlu'' :: (Arrow a) => TraversalArrow a (Int, (Bool, Bool)) (Bool)
aAlu'' 
    =   aId 
    *** (   aOr
        &&& aAnd
        )
    >>> aMuxF


--                                           a     b     cin    sum   cout
a1BitAdder :: (Arrow a) => TraversalArrow a (Bool, Bool, Bool) (Bool, Bool)
a1BitAdder
    =   arr (\(a, b, c) -> ((a, b), c))
    >>> first (aAnd &&& aOr)
    >>> arr (\((t1, t3), c) -> ((t1, c), t3))
    >>> first (aAnd &&& aOr)
    >>> arr (\((sum, t2), t3) -> (sum, (t2, t3)))
    >>> second aOr


test :: (Arrow a) => TraversalArrow a ((Bool, Bool)) Bool
test 
    =   first (arr (\x -> (x, x))) -- DOES THIS RESULT IN AN ERROR???
    >>> first aAnd
    >>> aAnd
