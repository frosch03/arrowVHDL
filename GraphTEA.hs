{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module GraphTEA where

import Control.Category 
import Prelude hiding (id, (.))
import Data.Bits (xor, shiftL, shiftR)

import GraphTraversal.Traversal
import GraphTraversal.Show
import GraphTraversal.Core
import GraphTraversal.Graph
import GraphTraversal.Auxillary

-- runTraversal_ aId (5)
-- runTraversal_ (aXor >>> aId) (1, 2)
-- runTraversal_ (aXor >>> (aId &&& (aConst 4)) >>> aXor)  (1, 2)
-- runTraversal_ aFeistelRound ((0, 0), (0, 0))

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


aFeistelRound :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
aFeistelRound 
    =   ( proc ((p0, p1), (k0, k1)) -> do
            tmp1 <- aShiftL4addKey -< (p1, k0)
            tmp2 <- aAddMagic      -< (p1)
            tmp3 <- aShiftR5addKey -< (p1, k1)

            tmp4 <- aXor           -< (tmp1, tmp2)
            tmp5 <- aXor           -< (tmp4, tmp3)

            erg0 <- returnA        -< (p1)
            erg1 <- aAdd           -< (p0, tmp5)
            returnA                -< (erg0, erg1)
        )

-- test 
--     = proc ((p0, p1), (k0, k1)) -> do
--         tmp1 <- aShiftL4addKey -< (p1, k0)
--         tmp2 <- aAddMagic      -< (p1)
--         tmp3 <- aShiftR5addKey -< (p1, k1)
-- 
--         tmp4 <- aXor           -< (tmp1, tmp2)
--         tmp5 <- aXor           -< (tmp4, tmp3)
-- 
--         erg0 <- returnA        -< (p1)
--         erg1 <- aAdd           -< (p0, tmp5)
--         returnA                -< (erg0, erg1)



g1 :: StructGraph
g1 = MkSG { name    = " G1 "
          , compID  = 0
          , nodes   = [ MkSG { name     = "G1_SUB1"
                             , compID   = 1
                             , nodes    = []
                             , edges    = []
                             , sinks    = [0,1,2]
                             , sources  = [0,1]
                             }
                      , MkSG { name     = "G1_SUB2"
                             , compID   = 2
                             , nodes    = []
                             , edges    = []
                             , sinks    = [0,1]
                             , sources  = [0]
                             }
                      ]
          , edges   = [ MkEdge { sourceInfo = (Nothing, 0)
                               , sinkInfo   = (Just 1, 0)
                               }
                      , MkEdge { sourceInfo = (Nothing, 1)
                               , sinkInfo   = (Just 1, 1)
                               }
                      , MkEdge { sourceInfo = (Nothing, 2)
                               , sinkInfo   = (Just 1, 2)
                               }
                      , MkEdge { sourceInfo = (Just 1, 0)
                               , sinkInfo   = (Just 2, 0)
                               }
                      , MkEdge { sourceInfo = (Just 1, 1)
                               , sinkInfo   = (Just 2, 1)
                               }
                      , MkEdge { sourceInfo = (Just 2, 0)
                               , sinkInfo   = (Nothing, 0)
                               }
                      ]
          , sinks   = [0,1,2]
          , sources = [0]
          }

g2 :: StructGraph
g2 = MkSG { name    = " G2 "
          , compID  = 0
          , nodes   = [ MkSG { name     = "G2_SUB1"
                             , compID   = 1
                             , nodes    = []
                             , edges    = []
                             , sinks    = [0]
                             , sources  = [0,1]
                             }
                      , MkSG { name     = "G2_SUB2"
                             , compID   = 2
                             , nodes    = []
                             , edges    = []
                             , sinks    = [0,1]
                             , sources  = [0]
                             }
                      ]
          , edges   = [ MkEdge { sourceInfo = (Nothing, 0)
                               , sinkInfo   = (Just 1, 0)
                               }
                      , MkEdge { sourceInfo = (Just 1, 0)
                               , sinkInfo   = (Just 2, 0)
                               }
                      , MkEdge { sourceInfo = (Just 1, 1)
                               , sinkInfo   = (Just 2, 1)
                               }
                      , MkEdge { sourceInfo = (Just 2, 0)
                               , sinkInfo   = (Nothing, 0)
                               }
                      ]
          , sinks   = [0]
          , sources = [0]
          }


aG1 :: (Arrow a) => TraversalArrow a (Int, Int, Int) (Int)
aG1
    = augment
        g1
    $ arr (\(x, y, z) -> x)

aG2 :: (Arrow a) => TraversalArrow a (Int) (Int)
aG2
    = augment
        g2 
    $ arr (\x -> x)


aDup :: (Arrow a) => TraversalArrow a b (b, b)
aDup
    = augment
        emptyGraph { name    = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   }
    $ arr (\(x) -> (x, x)) 

aTest :: (Arrow a) => TraversalArrow a Int Int
aTest = proc (x) -> do
    x' <- aAddMagic -< x
    returnA    -< x'

aBlub = (aDup >>> aAdd)
