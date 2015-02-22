{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module System.ArrowVHDL.Beispiel where

import Control.Category 
import Prelude hiding (id, (.))

import System.ArrowVHDL.Circuit.Arrow
import System.ArrowVHDL.Circuit.Auxillary
import System.ArrowVHDL.Circuit.Descriptor

-- import System.ArrowVHDL.TEA
import System.ArrowVHDL.Circuit.Workers

import System.ArrowVHDL.Circuit
import System.ArrowVHDL.Circuit.Defaults 
        ( aId
        , aConst
        , aDup
        , aFlip
        , aAdd
        , aXor
        , aShiftL, aShiftL4
        , aShiftR, aShiftR5
        , aXorMagic
        , aFst, aSnd
        )

aAdd1 :: (Arrow a) => a Int Int
aAdd1 = arr (\x -> x +1)

aSub1 :: (Arrow a) => a Int Int
aSub1 = arr (\x -> x -1)

aSub2 = aSub1 >>> aSub1



-- Beispiel 0
-------------
aTest0 
    =   aDup 
    >>> aAdd

netlist_Test0 :: CircuitDescriptor
netlist_Test0 
    = synthesize aTest0


-- Beispiel 1
-------------
aTest1 
    = proc (x) -> do
        tmp <- aAdd -< (x, x)
        returnA      -< tmp

_netlist_Test1 
    = synthesize aTest1 


-- Beispiel 2
-------------
aTest2 
    = proc (x1, x2) -> do
        tmp1 <- aDup -< x1
        tmp2 <- aDup -< x2
        tmp3 <- aAdd -< tmp1
        tmp4 <- aAdd -< tmp2
        tmp5 <- aAdd -< (tmp3, tmp4)
        returnA      -< tmp5

_netlist_Test2 
    = synthesize aTest2


-- Beispiel 3
-------------
aTest2' 
    = ((first aDup >>> arr (\ (tmp1, x2)   -> (x2,   tmp1))) >>> 
       (first aDup >>> arr (\ (tmp2, tmp1) -> (tmp1, tmp2))) >>> 
       (first aAdd >>> arr (\ (tmp3, tmp2) -> (tmp2, tmp3))) >>> 
       (first aAdd >>> arr (\ (tmp4, tmp3) -> (tmp3, tmp4))) >>> aAdd)

netlist_Test2' 
    = synthesize aTest2'


-- Beispiel 4
-------------
aShiftL4_XorKey
    =   first (   aDup
              >>> second (aConst 4)
              >>> aShiftL
              )
    >>> aXor

-- netlist_ShiftL4_XorKey 
--     = synthesize aShiftL4_XorKey


-- Beispiel 5
-------------
-- delta = 2654435769
-- aXorDelta
--     =   second (aConst delta)
--     >>> aXor

-- netlist_XorDelta 
--     = synthesize aXorDelta


-- Beispiel 6
-------------
aShiftR5_XorKey
    =   first (   aDup
              >>> second (aConst 5)
              >>> aShiftR
              )
    >>> aXor

-- netlist_ShiftR5_XorKey 
--     = synthesize aShiftR5_XorKey


counter :: (ArrowCircuit a) => a Int Int
counter = proc reset -> do 
            rec output <- (arr (+1)) -< reset
                next   <- delay 0    -< output
            returnA -< output


aLoopBsp :: (ArrowLoop a) => Grid a Int Int
aLoopBsp
    = loop (aAdd >>> (aId &&& (aConst 4)))

