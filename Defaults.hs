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

oneNodeGraph :: String -> StructGraph
oneNodeGraph s = emptyGraph { name = s }

aId :: (Arrow a) => TraversalArrow a b b
aId 
    = augment 
        emptyGraph { name    = "ID"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr id


aConst :: (Arrow a, Show b) => b -> TraversalArrow a c b
aConst x 
    = augment 
        emptyGraph { name    = "CONST_" ++ (show x)
                   , sinks   = mkPins 1 -- a sink is needed for the rewire-function to work properly (TODO: is this ok?)
                   , sources = mkPins 1
                   }
    $ arr (const x)


aXor :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aXor 
    = augment 
        emptyGraph { name    = "XOR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry xor) 


aFst :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aFst 
    = augment 
        emptyGraph { name    = "FST"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (fst) 


aSnd :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aSnd 
    = augment 
        emptyGraph { name    = "SND"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (snd) 


aShiftL :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftL 
    = augment
        emptyGraph { name    = "SHIFTL"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftL) 

aShiftR :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftR 
    = augment
        emptyGraph { name    = "SHIFTR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry shiftR) 

aAdd :: (Arrow a) => TraversalArrow a  (Int, Int) (Int)
aAdd 
    = augment
        emptyGraph { name    = "ADD"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }
    $ arr (uncurry (+))

aFlip :: (Arrow a) => TraversalArrow a (b, c) (c, b)
aFlip 
    = augment
         emptyGraph { name    = "FLIP"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\(x, y) -> (y, x))

aSwapSnd :: (Arrow a) => TraversalArrow a ((b, c), d) ((b, d), c)
aSwapSnd
    = augment
         emptyGraph { name    = "SWPSND"
                    , sinks   = mkPins 2
                    , sources = mkPins 2
                    }
    $ arr (\((x, y), z) -> ((x, z), y))

aShiftL4 :: (Arrow a) => TraversalArrow a Int Int
aShiftL4 
    = augment
        emptyGraph { name    = "SHIFTL4"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (flip shiftL 4)

aShiftR5 :: (Arrow a) => TraversalArrow a Int Int
aShiftR5 
    = augment
        emptyGraph { name    = "SHIFTR5"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (flip shiftR 5)

aShiftL4addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKey 
    =   first aShiftL4
    >>> aAdd

aShiftR5addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftR5addKey 
    =   first aShiftR5
    >>> aAdd

aAddMagic :: (Arrow a) => TraversalArrow a ValChunk Int
aAddMagic
    = augment 
        emptyGraph { name    = "ADDMAGIC"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }
    $ arr (\x -> (x, 2654435769)) >>> aAdd

aDup :: (Arrow a) => TraversalArrow a b (b, b)
aDup
    = augment
        emptyGraph { name    = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   }
    $ arr (\(x) -> (x, x)) 
