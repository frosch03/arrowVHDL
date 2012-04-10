{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module Circuit.Defaults where

import Control.Category 
import Prelude hiding (id, (.))
import Data.Bits (xor, shiftL, shiftR)

import Circuit

import Circuit.Grid

import Circuit.Arrow 

import Circuit.Auxillary
import Circuit.Descriptor
import Circuit.Graphs
import Circuit.Show


type KeyChunk = Int
type ValChunk = Int
type Key   = (KeyChunk, KeyChunk, KeyChunk, KeyChunk)
type KeyHalf = (KeyChunk, KeyChunk)
type Value = (ValChunk, ValChunk)


oneNodeCircuit :: String -> CircuitDescriptor
oneNodeCircuit s = emptyCircuit { nodeDesc = emptyNodeDesc { label = s } }

aId :: (Arrow a) => Grid a b b
aId 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "ID"
            , sinks   = mkPins 1
            , sources = mkPins 1 
            }
          , cycles  = 1
          , space   = 1
          }
    $ arr id


aConst :: (Arrow a, Show b) => b -> Grid a c b
aConst x 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc
            { label = "CONST_" ++ (show x)
            , sinks   = mkPins 1 -- a sink is needed for the rewire-function to work properly (TODO: is this ok?)
            , sources = mkPins 1
            }
          , cycles  = 0
          , space   = 1
          }
    $ arr (const x)


aXor :: (Arrow a) => Grid a (Int, Int) (Int)
aXor 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "XOR"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (uncurry xor) 


aFst :: (Arrow a) => Grid a (Int, Int) (Int)
aFst 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "FST"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (fst) 


aSnd :: (Arrow a) => Grid a (Int, Int) (Int)
aSnd 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc
            { label   = "SND"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (snd) 


aShiftL :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftL 
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "SHIFTL"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 6
          }
    $ arr (uncurry shiftL) 

aShiftR :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftR 
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "SHIFTR"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 6
          }
    $ arr (uncurry shiftR) 

aAdd :: (Arrow a) => Grid a  (Int, Int) (Int)
aAdd 
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "ADD"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (uncurry (+))

aFlip :: (Arrow a) => Grid a (b, c) (c, b)
aFlip 
    = augment
         emptyCircuit
           { nodeDesc = emptyNodeDesc 
             { label   = "FLIP"
             , sinks   = mkPins 2
             , sources = mkPins 2
             }
           , cycles  = 1
           , space   = 4
           }
    $ arr (\(x, y) -> (y, x))

aSwapSnd :: (Arrow a) => Grid a ((b, c), d) ((b, d), c)
aSwapSnd
    = augment
         emptyCircuit
           { nodeDesc = emptyNodeDesc 
             { label   = "SWPSND"
             , sinks   = mkPins 2
             , sources = mkPins 2
             }
           , cycles  = 1
           , space   = 6
           }
    $ arr (\((x, y), z) -> ((x, z), y))

aShiftL4 :: (Arrow a) => Grid a Int Int
aShiftL4 
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "SHIFTL4"
            , sinks   = mkPins 1
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 6
          }
    $ arr (flip shiftL 4)

aShiftR5 :: (Arrow a) => Grid a Int Int
aShiftR5 
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "SHIFTR5"
            , sinks   = mkPins 1
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 6
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


--- NOTE: Hier ist ein schönes Problem aufgetreten:
-- da weiter unten arr ... >>> aAdd verwendet wird, und arr ... vom Typ Arrow a ist
-- aber aAdd vom Typ Grid a ist, gibt's nen type-mismatch... entweder aAdd muss auf Arrow a
-- runtergebrochen werden, oder arr ... muss vorher schon in einen Grid gehoben werden :) 
-- 
-- So oder so, schön ;) 
--aAddMagic :: (Arrow a) => Grid a ValChunk Int
aAddMagic
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "ADDMAGIC"
            , sinks   = mkPins 1
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (\x -> (x, 2654435769)) >>> aAdd

aDup :: (Arrow a) => Grid a b (b, b)
aDup
    = augment
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "DUP"
            , sinks   = mkPins 1
            , sources = mkPins 2
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (\(x) -> (x, x)) 

aRegister :: (Arrow a) => Grid a b b 
aRegister 
    = augment 
        ( mkRegister $ emptyNodeDesc
                       { sinks   = mkPins 1
                       , sources = mkPins 1
                       }
        )
    $ arr id
        
