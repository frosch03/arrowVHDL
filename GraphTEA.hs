{-# LANGUAGE Arrows, FlexibleContexts #-}
module GraphTEA where

import Control.Arrow
import Data.Bits (xor, shiftL, shiftR)

import GraphTraversal

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
    = augment_f_SG 
        (id) 
        emptyGraph { name    = "ID"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }

aConst :: (Arrow a, Show b) => b -> TraversalArrow a c b
aConst x 
    = augment_f_SG
        (\_ -> x)
        emptyGraph { name    = "CONST_" ++ (show x)
                   , sinks   = mkPins 1 -- a sink is needed for the rewire-function to work properly (TODO: is this ok?)
                   , sources = mkPins 1
                   }

aXor :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aXor 
    = augment_f_SG 
        (uncurry xor) 
        emptyGraph { name    = "XOR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }


aShiftL :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftL 
    = augment_f_SG 
        (uncurry shiftL) 
        emptyGraph { name    = "SHIFTL"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }

aShiftR :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftR 
    = augment_f_SG 
        (uncurry shiftR) 
        emptyGraph { name    = "SHIFTR"
                   , sinks   = mkPins 2
                   , sources = mkPins 1
                   }

aAdd :: (Arrow a) => TraversalArrow a  (Int, Int) (Int)
aAdd 
    = augment_f_SG 
        (uncurry (+))
        emptyGraph { name    = "ADD"
                   , sinks   = mkPins 2
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

-- aShiftL4addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
-- aShiftL4addKey 
--     = augment_aTA_SG 
--         (   first (   arr (\x -> (x, 4))
--                   >>> aShiftL
--                   )
--         >>> aAdd
--         )
--         emptyGraph { name    = "SHIFTL4_ADD"
--                    , sinks   = [ (Nothing, 0)
--                                , (Nothing, 1)
--                                ]
--                    , sources = [ (Nothing, 0) ]
--                    }

-- aShiftR5addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
-- aShiftR5addKey 
--     = augment_aTA_SG
--         (   first (   arr (\x -> (x, 5))
--                   >>> aShiftR
--                   )
--         >>> aAdd
--         )
--         emptyGraph { name    = "SHIFTR5_ADD"
--                    , sinks   = [ (Nothing, 0)
--                                , (Nothing, 1)
--                                ]
--                    , sources = [ (Nothing, 0) ]
--                    }

aShiftL4 :: (Arrow a) => TraversalArrow a Int Int
aShiftL4 
    = augment_f_SG
        (flip shiftL 4)
        emptyGraph { name    = "SHIFTL4"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }

aShiftL4addKey' :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKey' 
    =   first aShiftL4
    >>> aAdd

aShiftL4addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKey 
    =   (   first (   aId &&& aConst 4
                  >>> aShiftL
                  )
        >>> aAdd
        )

aShiftL4addKeyClean :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKeyClean 
    = ( (       (   aId &&& aConst 4
                >>> aShiftL
                )
            *** aId
        )
        >>> aAdd
      )


aShiftR5addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftR5addKey 
    =   (   first (   aId &&& aConst 5
                  >>> aShiftR
                  )
        >>> aAdd
        )


aAddMagic :: (Arrow a) => TraversalArrow a ValChunk Int
aAddMagic 
    = augment_aTA_SG
        (   arr (\x -> (x, 2654435769))
        >>> aAdd
        )
        emptyGraph { name    = "ADDMAGIC"
                   , sinks   = mkPins 1
                   , sources = mkPins 1
                   }



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
         

-- -- feistelRound_a :: (Arrow arr) => arr ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
-- -- feistelRound_a =  
-- --     arr (\((p0, p1), (k0, k1)) 
-- --          -> ( p0
-- --             , ( ( ( shiftL4addKey_a (p1, k0)
-- --                   , addMagic_a p1
-- --                   )
-- --                 , shiftR5addKey_a (p1, k1)
-- --                 )
-- --               , p1
-- --               )
-- --             )
-- --         )
-- --     >>> arr id *** ((xor_a *** arr id) *** arr id) 
-- --     >>> arr id ***  (xor_a             *** arr id) 
-- --     >>> arr (\(p0, (tmp, p1)) -> ((p0, tmp), p1))
-- --     >>> add_a                          *** arr id
-- 
-- -- cycle_a :: (Arrow arr) => arr (Key, Value) (Value)
-- -- cycle_a =  
-- --     arr (\((k0, k1, k2, k3), (p0, p1)) 
-- --         -> (((p0, p1), (k0, k1)), (k2, k3)))
-- --     >>> feistelRound_a *** arr id
-- --     >>> feistelRound_a
-- -- 
-- -- feistelRound :: (Arrow a) => a (Value, KeyHalf) Value
-- -- feistelRound = proc ((p0, p1), (k0, k1)) -> do
-- --     tmp1 <- shiftL4addKey_a -< (p1, k0)
-- --     tmp2 <- addMagic_a     -< p1
-- --     tmp3 <- shiftR5addKey_a -< (p1, k1)
-- -- 
-- --     tmp4 <- xor_a -< (tmp1, tmp2)
-- --     tmp5 <- xor_a -< (tmp4, tmp3)
-- -- 
-- --     erg0 <- returnA -< p1
-- --     erg1 <- add_a -< (p0, tmp5)
-- -- 
-- --     returnA -< (erg0, erg1)
--    
-- aFeistelRound :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
-- aFeistelRound = augment def feistelRound
--     where def = emptyGraph { name = "FEISTEL_ROUND" }
--           
-- feistelRound :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
-- feistelRound = TR $ proc (((p0, p1), (k0, k1)), t0) -> do
--                       (tmp1, t1) <- _shiftL4addKey -< ((p1, k0), t0)
--                       (tmp2, t2) <- _addMagic      -< (p1, t1)
--                       (tmp3, t3) <- _shiftR5addKey -< ((p1, k1), t2)
--                     
--                       (tmp4, t4) <- _xor           -< ((tmp1, tmp2), t3)
--                       (tmp5, t5) <- _xor           -< ((tmp4, tmp3), t4)
--                      
--                       (erg0, t6) <- returnA        -< (p1, t5)
--                       (erg1, t7) <- _add           -< ((p0, tmp5), t6)
--                      
--                       returnA                     -< ((erg0, erg1), t7)
--     where def = emptyGraph { name = "FEISTEL_ROUND" }
--           (TR _shiftL4addKey) = aShiftL4addKey
--           (TR _shiftR5addKey) = aShiftR5addKey
--           (TR _addMagic)      = aAddMagic
--           (TR _xor)           = aXor
--           (TR _add)           = aAdd
--           
-- -- cycle :: (Arrow a) => a (Key, Value) Value
-- -- cycle = proc ((k0, k1, k2, k3), (p0, p1)) -> do
-- --     tmp <- feistelRound -< ((p0, p1), (k0, k1))
-- --     erg <- feistelRound -< (tmp     , (k2, k3))
-- --     returnA -< erg
--     


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
    = augment_f_SG 
        (\(x, y, z) -> x)
        g1

aG2 :: (Arrow a) => TraversalArrow a (Int) (Int)
aG2
    = augment_f_SG 
        (\x -> x)
        g2 


aDup :: (Arrow a) => TraversalArrow a b (b, b)
aDup
    = augment_f_SG 
        (\(x) -> (x, x)) 
        emptyGraph { name    = "DUP"
                   , sinks   = mkPins 1
                   , sources = mkPins 2
                   }

aShiftL4addKey_ :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKey_ 
    =     (   (   aId &&& aConst 4
              >>> aShiftL
              )
          *** aId
          )
      >>>
      aAdd


-- (aShiftL4 *** aId ) >>> aAdd
