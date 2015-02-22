{-# LANGUAGE Arrows, 
             NoMonomorphismRestriction,
             RebindableSyntax #-}
module System.ArrowVHDL.CRC 
where

import Prelude

import Control.Category ((>>>))

import System.ArrowVHDL.Circuit
import System.ArrowVHDL.Circuit.Arrow
import System.ArrowVHDL.Circuit.Auxillary
import System.ArrowVHDL.Circuit.Defaults

import Data.Bits

infixr 0 >:>

(>:>) aA aB = aA >>> (second aB)

mvRight :: (Arrow a) => Grid a (my, (b, rst)) (b, (my, rst))
mvRight
    =   aDistr
    >>> first aSnd



-- crc_polynom_ccitt ::  (Arrow a, Bits b) => Grid a (b, (b, (b, (b, b))))  (b, (b, (b, b)))
crc_polynom_ccitt ::  (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, Bool))))  (Bool, (Bool, (Bool, Bool)))
crc_polynom_ccitt 
    =   mvRight >:> mvRight >:>
        (   aDistr 
        >>> (aXor *** aXor)
        )

-- crc_polynom_ccitt 
--  = proc (x4, (x3, (x2, (x1, x0)))) -> do
--      o1 <- aXor -< (x4, x0)
--      o2 <- aXor -< (x4, x1)
--      o3 <- aId  -< (x2)
--      o4 <- aId  -< (x3)
--      returnA    -< (o4, (o3, (o2, o1)))

-- crc_polynom_usb ::   (Arrow a, Bits b) => Grid a (b, (b, (b, (b, (b, b)))))  (b, (b, (b, (b, b))))
crc_polynom_usb ::   (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))  (Bool, (Bool, (Bool, (Bool, Bool))))
crc_polynom_usb
    =   mvRight >:> mvRight >:>
        (   aDistr
        >>> aXor *** (mvRight >:> aXor)
        )

-- crc_polynom_usb
--  = proc (x5, (x4, (x3, (x2, (x1, x0))))) -> do
--      o1 <- aXor -< (x5, x0)
--      o2 <- aId  -< (x1)
--      o3 <- aXor -< (x5, x2)
--      o4 <- aId  -< (x3)
--      o5 <- aId  -< (x4)
--      returnA    -< (o5, (o4, (o3, (o2, o1))))

-- crc_polynom_sdmmc :: (Arrow a, Bits b) => Grid a (b, (b, (b, (b, (b, (b, (b, b)))))))  (b, (b, (b, (b, (b, (b, b))))))
crc_polynom_sdmmc :: (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))  (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))
crc_polynom_sdmmc
    =   mvRight >:> mvRight >:> mvRight >:>
        (   aDistr
        >>> aXor *** (mvRight >:> mvRight >:> aXor)
        )

-- crc_polynom_sdmmc
--  = proc (x7, (x6, (x5, (x4, (x3, (x2, (x1, x0))))))) -> do
--      o1 <- aXor -< (x7, x0)
--      o2 <- aId  -< (x1)
--      o3 <- aId  -< (x2)
--      o4 <- aXor -< (x7, x3)
--      o5 <- aId  -< (x4)
--      o6 <- aId  -< (x5)
--      o7 <- aId  -< (x6)
--      returnA    -< (o7, (o6, (o5, (o4, (o3, (o2, o1))))

    

--                                    3      2      1     0         2      1     0
-- inner_crc_3ord :: (Arrow a, Bits b) => Grid a (b, (b, (b, b)))  (b, (b, b))
inner_crc_3ord 
    =   aDistr
    >>> aSnd *** aDistr
    >>> second (aXor *** aXor)


toInner3
    =   mvRight 
    >:> mvRight 
    >:> aFlip

toInner4
    =   mvRight
    >:> mvRight 
    >:> mvRight 
    >:> aFlip

toInner5
    =   mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> aFlip

toInner7
    =   mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> aFlip

toInner8
    =   mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> mvRight
    >:> aFlip


-- crc_checksum_8 crc_polynom polySkip start rest padding
crc_checksum_8 crc_polynom polySkip start rest padding
    =   (padding &&& aId)
    >>> toInner8

    >>> (start &&& rest)
    >>> first (crc_polynom)

    >>> step
    >>> step
    >>> step
    >>> step
    >>> step
    >>> step

    >>> aFlip
    >>> polySkip
    >>> crc_polynom

    where step =   a_aBC2ABc
               >>> first 
                   (   aFlip
                   >>> polySkip
                   >>> crc_polynom
                   )


-- crc_test
--     = crc_checksum_8 
--         inner_crc_3ord
--         toInner3
--         (second (second (second aFst)))
--         (aSnd >>> aSnd >>> aSnd >>> aSnd)
--         (aConst (False, (False, False)))


-- crc_checksum_ccitt_8 ::  (Arrow a, Bits b) => Grid a (b, (b, (b, (b, (b, (b, (b, b))))))) (b, (b, (b, b)))
crc_checksum_ccitt_8 ::  (Arrow a) => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, Bool)))
crc_checksum_ccitt_8
    = crc_checksum_8   
        crc_polynom_ccitt
        toInner4
        (second.second.second.second $ aFst)
        (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)
        (aConst (False, (False, (False, False))))

-- crc_checksum_usb_8 ::   (Arrow a, Bits b) => Grid a (b, (b, (b, (b, (b, (b, (b, b))))))) (b, (b, (b, (b, b))))
-- crc_checksum_usb_8 
--     = crc_checksum_8 
--         crc_polynom_usb 
--         toInner5 
--         (second.second.second.second.second $ aFst)
--         (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)
--         (aConst (False, (False, (False, (False, False)))))

-- crc_checksum_sdmmc_8 :: (Arrow a, Bits b) => Grid a (b, (b, (b, (b, (b, (b, (b, b))))))) (b, (b, (b, (b, (b, (b, b))))))
-- crc_checksum_sdmmc_8
--     = crc_checksum_8 
--         crc_polynom_sdmmc
--         toInner7
--         (second.second.second.second.second.second.second $ aFst)
--         (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)
--         (aConst (False, (False, (False, (False, (False, (False, False)))))))

