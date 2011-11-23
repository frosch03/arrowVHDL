{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module Defaults where

import Control.Category 
import Prelude hiding (id, (.))
import Data.Bits (xor, shiftL, shiftR)

import GraphTraversal.Traversal
import GraphTraversal.Show
import GraphTraversal.Core
import GraphTraversal.Graph
import GraphTraversal.Auxillary


type KeyChunk = Int
type ValChunk = Int
type Key   = (KeyChunk, KeyChunk, KeyChunk, KeyChunk)
type KeyHalf = (KeyChunk, KeyChunk)
type Value = (ValChunk, ValChunk)

oneNodeGraph :: String -> Circuit
oneNodeGraph s = emptyGraph { label = s }

aId :: (Arrow a) => Grid a b b
aId 
    = augment 
        emptyGraph { label   = "ID"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr id


aConst :: (Arrow a, Show b) => b -> Grid a c b
aConst x 
    = augment 
        emptyGraph { label   = "CONST_" ++ (show x)
                   , sinks   = mkPins 1 -- a sink is needed for the rewire-function to work properly (TODO: is this ok?)
                   , sources = mkPins 1
                   }
    $ arr (const x)


aXor :: (Arrow a) => Grid a (Int, Int) (Int)
aXor 
    = augment 
        emptyGraph { label   = "XOR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry xor) 


aFst :: (Arrow a) => Grid a (Int, Int) (Int)
aFst 
    = augment 
        emptyGraph { label   = "FST"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (fst) 


aSnd :: (Arrow a) => Grid a (Int, Int) (Int)
aSnd 
    = augment 
        emptyGraph { label   = "SND"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (snd) 


aShiftL :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftL 
    = augment
        emptyGraph { label   = "SHIFTL"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftL) 

aShiftR :: (Arrow a) => Grid a (Int, Int) (Int)
aShiftR 
    = augment
        emptyGraph { label   = "SHIFTR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftR) 

aAdd :: (Arrow a) => Grid a  (Int, Int) (Int)
aAdd 
    = augment
        emptyGraph { label   = "ADD"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry (+))

aFlip :: (Arrow a) => Grid a (b, c) (c, b)
aFlip 
    = augment
         emptyGraph { label   = "FLIP"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\(x, y) -> (y, x))

aSwapSnd :: (Arrow a) => Grid a ((b, c), d) ((b, d), c)
aSwapSnd
    = augment
         emptyGraph { label   = "SWPSND"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\((x, y), z) -> ((x, z), y))

aShiftL4 :: (Arrow a) => Grid a Int Int
aShiftL4 
    = augment
        emptyGraph { label   = "SHIFTL4"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (flip shiftL 4)

aShiftR5 :: (Arrow a) => Grid a Int Int
aShiftR5 
    = augment
        emptyGraph { label   = "SHIFTR5"
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
        emptyGraph { label   = "ADDMAGIC"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (\x -> (x, 2654435769)) >>> aAdd

aDup :: (Arrow a) => Grid a b (b, b)
aDup
    = augment
        emptyGraph { label   = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   }
    $ arr (\(x) -> (x, x)) 
