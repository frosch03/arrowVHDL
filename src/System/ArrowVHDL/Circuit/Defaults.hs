{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module System.ArrowVHDL.Circuit.Defaults where

import Control.Category 
import Prelude hiding (id, (.))
import qualified Data.Bits as B -- (shiftL, shiftR, xor, (.&.))

import System.ArrowVHDL.Circuit

import System.ArrowVHDL.Circuit.Grid

import System.ArrowVHDL.Circuit.Arrow 

import System.ArrowVHDL.Circuit.Auxillary
import System.ArrowVHDL.Circuit.Descriptor
import System.ArrowVHDL.Circuit.Graphs
import System.ArrowVHDL.Circuit.Show


type KeyChunk = Int
type ValChunk = Int
type Key   = (KeyChunk, KeyChunk, KeyChunk, KeyChunk)
type KeyHalf = (KeyChunk, KeyChunk)
type Value = (ValChunk, ValChunk)

-- xor :: Bool -> Bool -> Bool
-- xor x y | x == True && y == False = True
--         | x == False && y == True = True
--         | otherwise = False

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


(.&.) :: Bool -> Bool -> Bool
True .&. True = True
_    .&. _    = False

(.|.) :: Bool -> Bool -> Bool
False .|. False = False
_     .|. _     = True

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False

-- shiftL8 :: (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool)))))))) 
--         -> Int 
--         -> (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool))))))))
-- shiftL8 (x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8)))))))) i 
--   | i == 0 
--   = (x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8))))))))
--   | i == 1
--   = (x2, (x3, (x4, (x5, (x6, (x7, (x8, (False))))))))
--   | i == 2 
--   = (x3, (x4, (x5, (x6, (x7, (x8, (False, (False))))))))
--   | i == 3 
--   = (x4, (x5, (x6, (x7, (x8, (False, (False, (False))))))))
--   | i == 4 
--   = (x5, (x6, (x7, (x8, (False, (False, (False, (False))))))))
--   | i == 5 
--   = (x6, (x7, (x8, (False, (False, (False, (False, (False))))))))
--   | i == 6 
--   = (x7, (x8, (False, (False, (False, (False, (False, (False))))))))
--   | i == 7 
--   = (x8, (False, (False, (False, (False, (False, (False, (False))))))))
--   | i == 8 
--   = (False, (False, (False, (False, (False, (False, (False, (False))))))))
-- shiftL = shiftL8

-- shiftR8 :: (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool)))))))) 
--         -> Int 
--         -> (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool))))))))
-- shiftR8 (x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8)))))))) i 
--   | i == 0 
--   = (x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8))))))))
--   | i == 1
--   = (False, (x1, (x2, (x3, (x4, (x5, (x6, (x7))))))))
--   | i == 2 
--   = (False, (False, (x1, (x2, (x3, (x4, (x5, (x6))))))))
--   | i == 3 
--   = (False, (False, (False, (x1, (x2, (x3, (x4, (x5))))))))
--   | i == 4 
--   = (False, (False, (False, (False, (x1, (x2, (x3, (x4))))))))
--   | i == 5 
--   = (False, (False, (False, (False, (False, (x1, (x2, (x3))))))))
--   | i == 6 
--   = (False, (False, (False, (False, (False, (False, (x1, (x2))))))))
--   | i == 7 
--   = (False, (False, (False, (False, (False, (False, (False, (x1))))))))
--   | i == 8 
--   = (False, (False, (False, (False, (False, (False, (False, (False))))))))
-- shiftR = shiftR8

-- aAnd :: (Arrow a, Bits b) => Grid a (b, b) (b)
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
    $ arr (uncurry (.&.)) 


-- aOr :: (Arrow a, Bits b) => Grid a (b, b) (b) -- :: (Arrow a) => Grid a (Bool, Bool) (Bool)
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
    $ arr (uncurry (.|.)) 


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


aBXor :: (Arrow a, B.Bits b) => Grid a (b, b) (b)    -- :: (Arrow a) => Grid a (Bool, Bool) (Bool)
aBXor 
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
    $ arr (uncurry B.xor)

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


-- aFst :: (Arrow a, Bits b) => Grid a (b, c) (b)
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


-- aSnd :: (Arrow a, Bits c) => Grid a (b, c) (c)
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


aShiftL :: (Arrow a, B.Bits b) => Grid a (b, Int) (b)
-- aShiftL :: (Arrow a) => Grid a ((Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))), Int) (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))
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
    $ arr (uncurry B.shiftL) 

aShiftR :: (Arrow a, B.Bits b) => Grid a (b, Int) (b)
-- aShiftR :: (Arrow a) => Grid a ((Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))), Int) (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))
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
    $ arr (uncurry B.shiftR) 

aAdd :: (Arrow a, Num b) => Grid a  (b, b) (b)
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

aAssocRight = a_ABc2aBC
aAssocLeft  = a_aBC2ABc

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


-- |'aDistr' defines an distributivity of an expression ... 
-- (x,(a,b))  ->  ((x,a), (x,b))

-- aDistr :: (Arrow a, Bits b, Bits c, Bits d) => Grid a (b, (c, d)) ((b, c), (b, d))
aDistr :: (Arrow a) => Grid a (b, (c, d)) ((b, c), (b, d))
aDistr 
    =   aDup
    >>> second aFst *** second aSnd

-- |'aDdistr' is the reverse operation to the Distr operation

-- aDdistr :: (Arrow a, Bits b, Bits c, Bits d, Bits e) => Grid a ((b, c), (d, e)) ((b, d), (c, e))
aDdistr :: (Arrow a) => Grid a ((b, c), (d, e)) ((b, d), (c, e))
aDdistr 
    =   aSwapSnd
    >>> a_aBC2ABc *** aId
    >>> a_ABc2aBC
    >>> aId *** aFlip

aShiftL4 :: (Arrow a, B.Bits b) => Grid a b b 
-- aShiftL4 :: (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))
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
    $ arr (flip B.shiftL 4)

aShiftR5 :: (Arrow a, B.Bits b) => Grid a b b
-- aShiftR5 :: (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))
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
    $ arr (flip B.shiftR 5)

-- aShiftL4addKey :: (Arrow a) => Grid a (ValChunk, KeyChunk) Int
-- aShiftL4addKey 
--     =   first aShiftL4
--     >>> aAdd

-- aShiftR5addKey :: (Arrow a) => Grid a (ValChunk, KeyChunk) Int
-- aShiftR5addKey 
--     =   first aShiftR5
--     >>> aAdd


--- NOTE: Hier ist ein schönes Problem aufgetreten:
-- da weiter unten arr ... >>> aAdd verwendet wird, und arr ... vom Typ Arrow a ist
-- aber aAdd vom Typ Grid a ist, gibt's nen type-mismatch... entweder aAdd muss auf Arrow a
-- runtergebrochen werden, oder arr ... muss vorher schon in einen Grid gehoben werden :) 
-- 
-- So oder so, schön ;) 
--aAddMagic :: (Arrow a) => Grid a ValChunk Int
aXorMagic
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
    $ arr (\x -> (x, 2654435769)) >>> aBXor

--aDup :: (Arrow a) => Grid a b (b, b)
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
