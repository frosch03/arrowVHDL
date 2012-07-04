{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module Circuit.Defaults where

import Control.Category 
import Prelude hiding (id, (.))
import Data.Bits (shiftL, shiftR)

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

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

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


aAnd :: (Arrow a) => Grid a (Bool, Bool) (Bool)
aAnd 
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "AND"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (uncurry (&&)) 


aOr :: (Arrow a) => Grid a (Bool, Bool) (Bool)
aOr
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "OR"
            , sinks   = mkPins 2
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 4
          }
    $ arr (uncurry (||)) 


aNot :: (Arrow a) => Grid a (Bool) (Bool)
aNot
    = augment 
        emptyCircuit 
          { nodeDesc = emptyNodeDesc 
            { label   = "NOT"
            , sinks   = mkPins 1
            , sources = mkPins 1
            }
          , cycles  = 1
          , space   = 2
          }
    $ arr (not) 


aXor :: (Arrow a) => Grid a (Bool, Bool) (Bool)
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


aFst :: (Arrow a) => Grid a (b, c) (b)
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


aSnd :: (Arrow a) => Grid a (b, c) (c)
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

a_ABc2aBC :: (Arrow a) => Grid a ((b, c), d) (b, (c, d))
a_ABc2aBC    
    = augment
         emptyCircuit
           { nodeDesc = emptyNodeDesc 
             { label   = "ABc2aBC"
             , sinks   = mkPins 2
             , sources = mkPins 2
             }
           , cycles  = 1
           , space   = 6
           }
    $ arr (\((x, y), z) -> (x, (y, z)))

a_aBC2ABc :: (Arrow a) => Grid a (b, (c, d)) ((b, c), d)
a_aBC2ABc    
    = augment
         emptyCircuit
           { nodeDesc = emptyNodeDesc 
             { label   = "aBC2ABc"
             , sinks   = mkPins 2
             , sources = mkPins 2
             }
           , cycles  = 1
           , space   = 6
           }
    $ arr (\(x, (y, z)) -> ((x, y), z))

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
        

-- aL_headtail :: (Arrow a) => Grid a ([b]) (b, [b])
-- aL_headtail 
--     = augment 
--         emptyCircuit 
--             { nodeDesc = emptyNodeDesc
--                 { label   = "listHEADTAIL"
--                 , sinks   = mkPins 1
--                 , sources = mkPins 2
--                 }
--             , cycles = 2
--             , space  = 16
--             }
--     $ arr (\(x:xs) -> (x, xs))
