{-# LANGUAGE Arrows, 
             NoMonomorphismRestriction,
             RebindableSyntax #-}
             -- GADTs #-}
module System.ArrowVHDL.TEA where

import Data.Char (digitToInt, ord, chr)
import qualified Data.Bits as B
import Data.Word (Word32(..))
import Prelude hiding (cycle)

import Control.Category
import System.ArrowVHDL.Circuit
import System.ArrowVHDL.Circuit.Arrow
import System.ArrowVHDL.Circuit.Defaults hiding (Value, Key)

type Key     = (Word32, Word32, Word32, Word32)
type HalfKey = (Word32, Word32)
type Value   = (Word32, Word32)
type Round   = Int

type TL4
    = (Bool, (Bool, (Bool, (Bool))))

type TL32 
    = (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, 
      (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, 
      (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool,
      (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool,  Bool
      )))))))))))))))))))))))))))))))
type KeyTL     = (TL32, TL32, TL32, TL32)
type HalfKeyTL = (TL32, TL32)
type ValueTL   = (TL32, TL32)
type RoundTL   = TL32

-- aXorTL4
--   =     aXor 
--     >:> aXor 
--     >:> aXor 
--     >:> aXor
--   where 
--     aA >:> aB = ((aFst *** aFst) >>> aA) &&& ((aSnd *** aSnd) >>> aB)
--     infixr 4 >:>

-- aXorTL32
--   =     aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor
--     >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor 
--     >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor 
--     >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor >:> aXor 
--   where 
--     aA >:> aB = ((aFst *** aFst) >>> aA) &&& ((aSnd *** aSnd) >>> aB)
--     infixr 4 >:>

-- aOrTL32
--   =     aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr
--     >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr 
--     >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr 
--     >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr >:> aOr 
--   where 
--     aA >:> aB = ((aFst *** aFst) >>> aA) &&& ((aSnd *** aSnd) >>> aB)
--     infixr 4 >:>

magicConstant = 0x9e3779b9

-- |`delta` is the function, that considering the round number,
-- calculates the magic number.
delta :: Int -> Word32
delta r = fromIntegral r * magicConstant


-- |Here comes a function `cycleN` that calculates the cypher of the
-- TEA. It therefore takes a round counter and calculates the TEA with
-- exact that amount of rounds.
cycleN :: Int -> Key -> Value -> Value
cycleN n = cycleN' (1, n)

-- |Another more specific version of `cycleN` is `cycleN'`. This
-- function has a more complex round counter (Tuple of Int vs. single
-- Int). The first element of the tuple is the actual round counter;
-- the second element count's backward and acts as the recursion
-- break.
cycleN' :: (Round, Int) -> Key -> Value -> Value
cycleN' (_,0) _ v = v
cycleN' (r,n) k v = cycleN' (r+1, n-1) k (cycle r k v)

-- |`cycle` without any naming suffixes is the function that actually
-- calculates one round of the TEA.
cycle :: Round -> Key -> Value -> Value
cycle r (k0, k1, k2, k3) value
  = let value' = feistel1 value
    in           feistel2 value'
  where feistel1 = feistel r (k0, k1)
        feistel2 = feistel r (k2, k3)

-- |The `feistel` function contains the logic of the TEA. It is
-- similar to the depicted definition inside the wikipedia-article on
-- TEA: https://en.wikipedia.org/wiki/Tiny_Encryption_Algorithm
feistel :: Round -> HalfKey -> Value -> Value
feistel r (kA, kB) (vA, vB)
  = (vB, vA + tB)
  where t1 = kA + (vB `B.shift`    4)
        t2 = vB + delta r
        t3 = kB + (vB `B.shift` (-5))
        tB = t1 `B.xor` t2 `B.xor` t3

-- |The `feistel'functional` function contains the logic of the TEA. It is
-- similar to the depicted definition inside the wikipedia-article on
-- TEA: https://en.wikipedia.org/wiki/Tiny_Encryption_Algorithm
feistel'functional :: Round -> HalfKey -> Value -> Value
feistel'functional r (kA, kB) (vA, vB)
  = (vB, vA + tB)
  where t1 = kA + (vB `B.shift`    4)
        t2 = vB + delta r
        t3 = kB + (vB `B.shift` (-5))
        tB = t1 `B.xor` t2 `B.xor` t3

feistel'arrow :: Round -> HalfKey -> Value -> Value
feistel'arrow r key val
  = runGrid aTEA $ (key, val)
  where aTEA :: (Arrow a) => Grid a (HalfKey, Value) Value                  -- ((K0, K1), (V0, V1))
        aTEA =   aDistr                                                     -- (((K0, K1), V0), ((K0, K1), V1))
             >>> first                                                      
                 (   aSnd                                                   -- (V0, ((K0, K1), V1))
                 )
             >>> second
                 ( (     aFlip -- &&& aSnd                                  -- (V0, ((V1, (K0, K1)), V1))
                     >>> aDistr &&& aFst                                    -- (V0, ((((V1, K0), (V1, K1)), V1), V1))
                     >>> (   ((first aSHL4)
                         *** (first aSHR5))
                         *** aXorDeltaR
                         )                                                  -- (V0, ((((S1, K0), (S1, K1)), T3), V1))
                     >>> first (aXor *** aXor)                              -- (V0, (((T1, T2), T3), V1))
                     >>> first aXor                                         -- (V0, ((T12, T3), V1))
                     >>> aXor                                               -- (V0, (TB, V1))
                   )
                   &&& aSnd
                 )
             >>> a_aBC2ABc
             >>> first aXor
             >>> aFlip
        aSHL4 :: (Arrow a, B.Bits b) => Grid a b b
        aSHL4 = (aId &&& (aConst (4 :: Int))) >>> aShiftL

        aSHR5 :: (Arrow a, B.Bits b) => Grid a b b
        aSHR5 = (aId &&& (aConst (5 :: Int))) >>> aShiftR

        aXorDeltaR :: (Arrow a) => Grid a Word32 Word32
        aXorDeltaR = (aId &&& (aConst (delta r))) >>> aXor


-- key :: Key
-- key   = (65, 65, 65, 65) -- AAAA

-- testMessage = "Hello World!"

-- msg2vals :: String -> [Value]
-- msg2vals []          = []
-- msg2vals (s0:[])     = msg2vals [s0, '\0']
-- msg2vals (s0:s1:ses)
--   = (fromIntegral $ ord s0, fromIntegral $ ord s1) : (msg2vals ses)

-- vals2msg :: [Value] -> String
-- vals2msg [] = ""
-- vals2msg ((v0,v1):vs) = (chr (fromIntegral v0): (chr (fromIntegral v1) : (vals2msg vs)))

-- aFeistel 
--   = proc ((v0, v1), (k0, k1)) -> do
--       vsl <- aShl -< (v1, 4)
--       vsr <- aShr -< (v1, 5)

--       t1  <- aXor -< (k0, vsl)
--       t2  <- aXor -< (d,  v1)
--       t3  <- aXor -< (k1, vsr)

--       t4  <- aOr  -< (t1, t2)
--       t5  <- aOr  -< (t4, t3)

--       v0' <- aId  -< (v1)
--       v1' <- aXor -< (v0, t5)
--       returnA    -< (v0', v1')
--   where 
--     aShl = aShiftL
--     aShr = aShiftR
--     d    = (delta 0)

-- aCycle 
--   = proc ((v0, v1), ((k0, k1), (k2, k3))) -> do
--       (v0', v1')   <- aFeistel -< ((v0, v1),   (k0, k1))
--       (v0'', v1'') <- aFeistel -< ((v0', v1'), (k2, k3))
--       returnA                  -< (v0'', v1'')


aShiftL4_XorKey
  =   first 
      (     aDup
        >>> second (aConst 4)
        >>> aShiftL
      )
    >>> aAdd
    -- >>> aXor

aShiftR5_XorKey
  =   first 
      (     aDup
        >>> second (aConst 5)
        >>> aShiftR
      )
    >>> aAdd
    -- >>> aXor

aXorDelta
    =   aId &&& (aConst 2654435769)
    >>> aAdd
    -- >>> aXor

aFeistelTest
  = (aFst >>> aSnd) &&& (aFst >>> aFst) &&& (first aSnd)
   >>> second 
       ( second 
         (   aDistr &&& aFst
         >>> (aShiftL4_XorKey *** aShiftR5_XorKey) *** aXorDelta
         >>> first aOr
         >>> aOr
         )
       )
   >>> second aAdd

aCycleTest
  =    a_aBC2ABc
   >>> first aFeistelTest
   >>> aFeistelTest

