{-# LANGUAGE Arrows,
             NoMonomorphismRestriction,
             RebindableSyntax #-}
module CRC 
where

import Prelude

import Control.Category ((>>>))

import Circuit
import Circuit.Arrow
import Circuit.Defaults

infixr 0 >:>

(>:>) aA aB = aA >>> (second aB)

mvRight :: (Arrow a) => Grid a (my, (b, rst)) (b, (my, rst))
mvRight
    =   aAssoc
    >>> first aSnd



crc_polynom_ccit ::  Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, Bool))))  (Bool, (Bool, (Bool, Bool)))
crc_polynom_ccit 
    =   mvRight >:> mvRight >:>
        (   aAssoc 
        >>> (aXor *** aXor)
        )

-- crc_polynom_ccit 
--  = proc (x4, (x3, (x2, (x1, x0)))) -> do
--      o1 <- aXor -< (x4, x0)
--      o2 <- aXor -< (x4, x1)
--      o3 <- aId  -< (x2)
--      o4 <- aId  -< (x3)
--      returnA    -< (o4, (o3, (o2, o1)))

crc_polynom_usb ::   Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))  (Bool, (Bool, (Bool, (Bool, Bool))))
crc_polynom_usb
    =   mvRight >:> mvRight >:>
        (   aAssoc
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

crc_polynom_sdmmc :: Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool)))))))  (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))
crc_polynom_sdmmc
    =   mvRight >:> mvRight >:> mvRight >:>
        (   aAssoc
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
inner_crc_3ord :: Arrow a => Grid a (Bool, (Bool, (Bool, Bool)))  (Bool, (Bool, Bool))
inner_crc_3ord 
    =   aAssoc
    >>> aSnd *** aAssoc
    >>> second (aXor *** aXor)


toInner3
    = mvRight >>> next
    ( mvRight >>> end )
    where next = second
          end  = second aFlip

toInner4
    = mvRight >>> next
    ( mvRight >>> next 
    ( mvRight >>> end ))
    where next = second
          end  = second aFlip

toInner5
    = mvRight >>> next
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> end )))
    where next = second
          end  = second aFlip

toInner7
    = mvRight >>> next
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> end )))))
    where next = second
          end  = second aFlip

toInner8
    = mvRight >>> next
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> next 
    ( mvRight >>> end ))))))
    where next = second
          end  = second aFlip


-- crc_checksum_8 crc_polynom polySkip padding start rest
--     = proc 
crc_checksum_8 crc_polynom polySkip padding start rest
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


crc_test
    = crc_checksum_8 
        inner_crc_3ord
        toInner3
        (aConst (False, (False, False)))
        (second (second (second aFst)))
        (aSnd >>> aSnd >>> aSnd >>> aSnd)

crc_checksum_ccit_8 ::  Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, Bool)))
crc_checksum_ccit_8
    = crc_checksum_8   
        crc_polynom_ccit
        toInner4
        (aConst (False, (False, (False, False))))
        (second.second.second.second $ aFst)
        (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)

crc_checksum_usb_8 ::   Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, (Bool, Bool))))
crc_checksum_usb_8 
    = crc_checksum_8 
        crc_polynom_usb 
        toInner5 
        (aConst (False, (False, (False, (False, False)))))
        (second.second.second.second.second $ aFst)
        (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)

crc_checksum_sdmmc_8 :: Arrow a => Grid a (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))) (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, Bool))))))
crc_checksum_sdmmc_8
    = crc_checksum_8 
        crc_polynom_sdmmc
        toInner7
        (aConst (False, (False, (False, (False, (False, (False, False)))))))
        (second.second.second.second.second.second.second $ aFst)
        (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd)

