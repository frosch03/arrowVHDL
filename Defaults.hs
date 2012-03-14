{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module Defaults where

import Control.Category 
import Prelude hiding (id, (.))
import Data.Bits (xor, shiftL, shiftR)

import Grid.Traversal
import Grid.Show
import Grid.Core
import Grid.Graph
import Grid.Auxillary


type KeyChunk = Int
type ValChunk = Int
type Key   = (KeyChunk, KeyChunk, KeyChunk, KeyChunk)
type KeyHalf = (KeyChunk, KeyChunk)
type Value = (ValChunk, ValChunk)

oneNodeCircuit :: String -> CircuitDescriptor
oneNodeCircuit s = emptyCircuit { label = s }

aId :: (Arrow a) => Grid a b b
aId 
    = augment 
        emptyCircuit { label   = "ID"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr id


aConst :: (Arrow a, Show b) => b -> Grid a c b
aConst x 
    = augment 
        emptyCircuit { label   = "CONST_" ++ (show x)
                   , sinks   = mkPins 1 -- a sink is needed for the rewire-function to work properly (TODO: is this ok?)
                   , sources = mkPins 1
                   }
    $ arr (const x)


aXor :: (Arrow a) => Grid a (Int, Int) (Int)
aXor 
    = augment 
        emptyCircuit { label   = "XOR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry xor) 


aFst :: (Arrow a) => Grid a (Int, Int) (Int)
aFst 
    = augment 
        emptyCircuit { label   = "FST"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (fst) 


aSnd :: (Arrow a) => Grid a (Int, Int) (Int)
aSnd 
    = augment 
        emptyCircuit { label   = "SND"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (snd) 


aShiftL :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftL 
    = augment
        emptyCircuit { label   = "SHIFTL"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftL) 

aShiftR :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftR 
    = augment
        emptyCircuit { label   = "SHIFTR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftR) 

aAdd :: (Arrow a) => Grid a  (Int, Int) (Int)
aAdd 
    = augment
        emptyCircuit { label   = "ADD"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry (+))

aFlip :: (Arrow a) => Grid a (b, c) (c, b)
aFlip 
    = augment
         emptyCircuit { label   = "FLIP"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\(x, y) -> (y, x))

aSwapSnd :: (Arrow a) => Grid a ((b, c), d) ((b, d), c)
aSwapSnd
    = augment
         emptyCircuit { label   = "SWPSND"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\((x, y), z) -> ((x, z), y))

aShiftL4 :: (Arrow a) => Grid a Int Int
aShiftL4 
    = augment
        emptyCircuit { label   = "SHIFTL4"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (flip shiftL 4)

aShiftR5 :: (Arrow a) => Grid a Int Int
aShiftR5 
    = augment
        emptyCircuit { label   = "SHIFTR5"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (flip shiftR 5)

aShiftL4addKey :: (Arrow a) => Grid a (ValChunk, KeyChunk) Int
aShiftL4addKey 
    =   first aShiftL4
    >>> aAdd

aShiftR5addKey :: (Arrow a) => Grid a (ValChunk, KeyChunk) Int
aShiftR5addKey 
    =   first aShiftR5
    >>> aAdd

aAddMagic :: (Arrow a) => Grid a ValChunk Int
aAddMagic
    = augment 
        emptyCircuit { label   = "ADDMAGIC"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (\x -> (x, 2654435769)) >>> aAdd

aDup :: (Arrow a) => Grid a b (b, b)
aDup
    = augment
        emptyCircuit { label   = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   }
    $ arr (\(x) -> (x, x)) 
