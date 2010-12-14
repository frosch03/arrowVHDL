{-# LANGUAGE Arrows #-}
module TEA_TEST where

import Control.Arrow
import Data.Bits (xor, shiftL, shiftR)

import Traversal



type KeyChunk = Int
type ValChunk = Int
type Key   = (KeyChunk, KeyChunk, KeyChunk, KeyChunk)
type KeyHalf = (KeyChunk, KeyChunk)
type Value = (ValChunk, ValChunk)




aId :: (Arrow a) => TraversalArrow a (b) (b)
aId =  saugmentStructFunct def (id)
    where def = emptyStructure { name = "ID" }

-- xor_a :: (Arrow arr) => arr (Int, Int) (Int)
-- xor_a =  arr (uncurry xor)

aXor :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aXor =  saugmentStructFunct def (uncurry xor)
    where def = emptyStructure { name = "XOR" }

-- shiftL_a :: (Arrow arr) => arr (Int, Int) (Int)
-- shiftL_a =  arr (uncurry shiftL)

aShiftL :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftL =  saugmentStructFunct def (uncurry shiftL)
    where def = emptyStructure { name = "SHIFT_L" }

-- shiftR_a :: (Arrow arr) => arr (Int, Int) (Int)
-- shiftR_a =  arr (uncurry shiftR)

aShiftR :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aShiftR =  saugmentStructFunct def (uncurry shiftR)
    where def = emptyStructure { name = "SHIFT_R" }

-- add_a :: (Arrow arr) => arr (Int, Int) (Int)
-- add_a =  arr (uncurry (+))

aAdd :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
aAdd =  saugmentStructFunct def (uncurry (+))
    where def = emptyStructure { name = "ADD" }

-- flip_a :: (Arrow arr) => arr (a, b) (b, a)
-- flip_a =  arr (\(x, y) -> (y, x))

aFlip :: (Arrow a) => TraversalArrow a (b, c) (c, b)
aFlip =  saugmentStructFunct def (\(x, y) -> (y, x))
    where def = emptyStructure { name = "FLIP" }


-- ***********************
-- *** Just some Tests ***
-- ***********************
aF :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aF  = proc (v, k) -> do
            v' <- aShiftL -< (v,  4)
            y  <- aAdd    -< (v', k)
            y' <- aId     -< (y)
            returnA       -< y'



aF1 :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aF1  = ((arr (\x -> (x, 4)) >>> aShiftL) *** aId) >>> aAdd

aF2 :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aF2  = saugmentSubcomponent def f 
    where def = emptyStructure { name = "FancyNameHere" }
          f   =   (arr (\x -> (x, 4)) >>> aShiftL) 
              *** 
                  (aId)
              >>> aAdd

aF3 :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aF3  = saugmentSubcomponent def aF1
    where def = emptyStructure { name = "FancyNameHere" }






-- shiftL4addKey_a :: (Arrow arr) => arr (ValChunk, KeyChunk) Int
-- shiftL4addKey_a = arr (\(v, k) -> add_a (k, shiftL_a (v, 4)))

aShiftL4addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftL4addKey = saugmentSubcomponent def f 
    where def  = emptyStructure { name = "SHIFT_L4_ADD_KEY" }
          f   :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
          f    =   (arr (\x -> (x, 4)) >>> aShiftL) *** aId
               >>> aAdd

-- shiftR5addKey_a :: (Arrow arr) => arr (ValChunk, KeyChunk) Int
-- shiftR5addKey_a = arr (\(v, k) -> add_a (k, shiftR_a (v, 5)))

aShiftR5addKey :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
aShiftR5addKey = saugmentSubcomponent def f
    where def  = emptyStructure { name = "SHIFT_R5_ADD_KEY" }
          f   :: (Arrow a) => TraversalArrow a (ValChunk, KeyChunk) Int
          f    =   (arr (\x -> (x, 5)) >>> aShiftR) *** aId
               >>> aAdd

-- addMagic_a :: (Arrow arr) => arr ValChunk Int
-- addMagic_a = arr (\x -> add_a (x, 2654435769))

aAddMagic :: (Arrow a) => TraversalArrow a ValChunk Int
aAddMagic =  saugmentSubcomponent def f
    where def = emptyStructure { name = "ADD_MAGIC" }
          f  :: (Arrow a) => TraversalArrow a ValChunk Int
          f   = arr (\x -> (x, 2654435769)) >>> aAdd




-- feistelRound_a :: (Arrow arr) => arr ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
-- feistelRound_a =  
--     arr (\((p0, p1), (k0, k1)) 
--          -> ( p0
--             , ( ( ( shiftL4addKey_a (p1, k0)
--                   , addMagic_a p1
--                   )
--                 , shiftR5addKey_a (p1, k1)
--                 )
--               , p1
--               )
--             )
--         )
--     >>> arr id *** ((xor_a *** arr id) *** arr id) 
--     >>> arr id ***  (xor_a             *** arr id) 
--     >>> arr (\(p0, (tmp, p1)) -> ((p0, tmp), p1))
--     >>> add_a                          *** arr id

aFeistelRound :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
aFeistelRound = saugmentSubcomponent def f
    where def = emptyStructure { name = "FEISTEL_ROUND" }
          f  :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
          f   = arr (\((p0, p1), (k0, k1))
                     -> ( p0
                        , ( ( ( (p1, k0)
                              , (p1)
                              )
                            , (p1, k1)
                            )
                          , p1
                          )
                        )
                    ) 

                >>> aId *** (((aShiftL4addKey *** aAddMagic) *** aShiftR5addKey) *** aId)
                >>> aId *** ((aXor *** aId) *** aId) 
                >>> aId ***  (aXor *** aId) 
                >>> arr (\(p0, (tmp, p1)) -> ((p0, tmp), p1))
                >>> aAdd *** aId 

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
-- aFeistelRound = saugmentSubcomponent def feistelRound
--     where def = emptyStructure { name = "FEISTEL_ROUND" }
--           
feistelRound :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
feistelRound = saugmentSubcomponent def f
    where def = emptyStructure { name = "FEISTEL_ROUND" }
          f :: (Arrow a) => TraversalArrow a ((ValChunk, ValChunk), (KeyChunk, KeyChunk)) (ValChunk, ValChunk)
          f                   = proc ((p0, p1), (k0, k1)) -> do
                                      tmp1 <- aShiftL4addKey -< (p1, k0)
                                      tmp2 <- aAddMagic      -< p1
                                      tmp3 <- aShiftR5addKey -< (p1, k1)
                                    
                                      tmp4 <- aXor           -< (tmp1, tmp2)
                                      tmp5 <- aXor           -< (tmp4, tmp3)
                                    
                                      erg0 <- returnA        -< p1
                                      erg1 <- aAdd           -< (p0, tmp5)
                                     
                                      returnA                -< (erg0, erg1)
--           
-- -- cycle :: (Arrow a) => a (Key, Value) Value
-- -- cycle = proc ((k0, k1, k2, k3), (p0, p1)) -> do
-- --     tmp <- feistelRound -< ((p0, p1), (k0, k1))
-- --     erg <- feistelRound -< (tmp     , (k2, k3))
-- --     returnA -< erg
--     
