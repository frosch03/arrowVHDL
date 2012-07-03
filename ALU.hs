{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module ALU 
where

import Prelude hiding (id, (.))

import Control.Category -- here we get >>> ...

import Circuit
import Circuit.Arrow -- here we get first and second
import Circuit.Defaults


-- aFst
-- aSnd
-- aXor
-- aShiftL
-- aShiftR
-- aAdd


-- |'aAssoc' defines an associativity of an expression ... 
-- (x,(a,b))  ->  ((x,a), (x,b))
aAssoc :: (Arrow a) => Grid a (b, (c, d)) ((b, c), (b, d))
aAssoc 
    =   aDup
    >>> second aFst *** second aSnd


-- | With 'aMux' a 1 Bit multiplexer is defined
aMux :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool)
aMux 
    =   aAssoc
    >>> aFlip *** aFlip
    >>> first (second aNot)
    >>> aAnd *** aAnd
    >>> aOr

-- |'aXum' is the Multiplexer where the last input-pin is the s-line
aXum =   aFlip 
     >>> aMux


a1Bit_MuxOp :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool)
a1Bit_MuxOp 
    =   second (   aDup
               >>> aOr *** aAnd
               )
    >>> aMux

a2Mux :: (Arrow a) => Grid a ((Bool, Bool), ((Bool, Bool), (Bool, Bool))) (Bool)
a2Mux 
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second
        (   first aFst
        >>> aAssoc
        >>> aMux *** aMux
        )
    >>> aMux


a3Mux :: (Arrow a) => Grid a ((Bool, (Bool, Bool)), (((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool)))) (Bool)
a3Mux
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second 
        (   first aFst
        >>> aAssoc
        >>> aAssoc *** aAssoc
        >>> (aMux *** aMux ) *** (aMux *** aMux) 
        )
    >>> a2Mux

a4Mux :: (Arrow a) => Grid a ((Bool, (Bool, (Bool, Bool))), ((((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool))), (((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool))))) (Bool)
a4Mux 
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second
        (   first aFst
        >>> aAssoc
        >>> aAssoc *** aAssoc
        >>> (aAssoc *** aAssoc) *** (aAssoc *** aAssoc)
        >>> ((aMux *** aMux ) *** (aMux *** aMux)) *** ((aMux *** aMux ) *** (aMux *** aMux))
        )
    >>> a3Mux


aFullAdd :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool, Bool)
aFullAdd 
    =   aFst &&& (   aSnd
                 >>> aXor &&& aAnd
                 )
    >>> _aFlipBrc
    >>> first (aXor &&& aAnd)
    >>> first (aFlip)
    >>> aSwapSnd
    >>> aFlip
    >>> second aOr
    where _aFlipBrc = aFlip >>> aSwapSnd >>> first aFlip


a1BitALU :: (Arrow a) => Grid a ((Bool, (Bool)), (Bool, Bool)) (Bool, Bool)
a1BitALU 
    =   aDup
    >>> first 
        (   aSwapSnd
        >>> aFst
        >>> aFullAdd
        )
    >>> second (aSnd *** aAnd)
    >>> aDup
    >>> first 
        (   second aSnd
        >>> first  aFlip
        >>> aSwapSnd
        >>> aFlip
        >>> aMux
        )
    >>> second 
        (   aFst
        >>> aFst
        )
    >>> aFlip



--     >>> (aFst *** aFst) &&& (aSnd *** aSnd)
--     >>> first aFlip
--     >>> aSwapSnd
--     >>> first aMux
--     >>> aFlip
