{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module ALU 
where

import Prelude hiding (id, (.))

import Control.Category -- here we get >>> ...

import Circuit
import Circuit.Arrow -- here we get first and second
import Circuit.Defaults


-- aFst
-- aSnd
-- aXor
-- aShiftL
-- aShiftR
-- aAdd


-- |'aAssoc' defines an associativity of an expression ... 
-- (x,(a,b))  ->  ((x,a), (x,b))
aAssoc :: (Arrow a) => Grid a (b, (c, d)) ((b, c), (b, d))
aAssoc 
    =   aDup
    >>> second aFst *** second aSnd


-- | With 'aMux' a 1 Bit multiplexer is defined
aMux :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool)
aMux 
    =   aAssoc
    >>> aFlip *** aFlip
    >>> first (second aNot)
    >>> aAnd *** aAnd
    >>> aOr

a2Mux :: (Arrow a) => Grid a ((Bool, Bool), ((Bool, Bool), (Bool, Bool))) (Bool)
a2Mux 
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second
        (   first aFst
        >>> aAssoc
        >>> aMux *** aMux
        )
    >>> aMux


a3Mux :: (Arrow a) => Grid a ((Bool, (Bool, Bool)), (((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool)))) (Bool)
a3Mux
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second 
        (   first aFst
        >>> aAssoc
        >>> aAssoc *** aAssoc
        >>> (aMux *** aMux ) *** (aMux *** aMux) 
        )
    >>> a2Mux

a4Mux :: (Arrow a) => Grid a ((Bool, (Bool, (Bool, Bool))), ((((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool))), (((Bool, Bool), (Bool, Bool)), ((Bool, Bool), (Bool, Bool))))) (Bool)
a4Mux 
    =   aDup
    >>> first (aFst >>> aSnd)
    >>> second
        (   first aFst
        >>> aAssoc
        >>> aAssoc *** aAssoc
        >>> (aAssoc *** aAssoc) *** (aAssoc *** aAssoc)
        >>> ((aMux *** aMux ) *** (aMux *** aMux)) *** ((aMux *** aMux ) *** (aMux *** aMux))
        )
    >>> a3Mux


-- |'anXum' is the Multiplexer where the last input-pin is the s-line
-- |it is generated out of one of the mux'es
anXum mux =   aFlip 
          >>> mux

aXum  = anXum aMux
a2Xum = anXum a2Mux
a3Xum = anXum a3Mux
a4Xum = anXum a4Mux


a1Bit_MuxOp :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool)
a1Bit_MuxOp 
    =   second (   aDup
               >>> aOr *** aAnd
               )
    >>> aMux

--                               Cin     a     b       e    Cout
aFullAdd :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool, Bool)
aFullAdd 
    =   aFst &&& (   aSnd
                 >>> aXor &&& aAnd
                 )
    >>> _aFlipBrc
    >>> first (aXor &&& aAnd)
    >>> first (aFlip)
    >>> aSwapSnd
    >>> first aOr
    >>> aFlip
    where _aFlipBrc = aFlip >>> aSwapSnd >>> first aFlip


type Input   = (Bool, Bool)
type Output  = Bool
type Cin     = Bool
type Cout    = Bool
type Opt1Bit = Bool
type Opt2Bit = (Opt1Bit, Opt1Bit)

type In2Bit  = (Input,  (Input))
type Out2Bit = (Output, (Output))

type In4Bit  = (Input,  (Input,  (Input,  (Input))))
type Out4Bit = (Output, (Output, (Output, (Output))))

type In8Bit  = (Input,  (Input,  (Input,  (Input, (Input,  (Input,  (Input,  (Input))))))))
type Out8Bit = (Output, (Output, (Output, (Output, (Output, (Output, (Output, (Output))))))))


aDassoc :: (Arrow a) => Grid a ((b, c), (d, e)) ((b, d), (c, e))
aDassoc 
    =   aSwapSnd
    >>> a_aBC2ABc *** aId
    >>> a_ABc2aBC
    >>> aId *** aFlip


noCarry :: (Arrow a) => (Grid a Input Output) -> Grid a (Cin, Input) (Cout, Output)
noCarry arrow = (aConst False) *** arrow


a2HullALU :: (Arrow a) 
          => Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Opt1Bit, (Cin, Input)) (Cout, Output)
a2HullALU h1 h2
    =   second (h1 &&& h2 >>> aDassoc)
    >>> aAssoc
    >>> aMux *** aMux

aAndOr :: (Arrow a) => Grid a (Opt1Bit, (Cin, Input)) (Cout, Output)
aAndOr = a2HullALU (noCarry aAnd) (noCarry aOr)

aXorNot :: (Arrow a) => Grid a (Opt1Bit, (Cin, Input)) (Cout, Output)
aXorNot = a2HullALU (noCarry aXor) (noCarry $ aFst >>> aNot)

eval :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) -> Grid a (Cin, (Opt1Bit, (Input, rest))) (Output, (Cout, (Opt1Bit, rest)))
eval aALU 
    =   second aAssoc
    >>> a_aBC2ABc
    >>> first aALU
    >>> a_ABc2aBC



alALU :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
                   -> Grid a (Cin, (Opt1Bit, (Input,  (Input)))) 
                                             (Output, (Output, (Cout)))
alALU aALU 
    = eval aALU >>> next aALU
    where next = second



a4BitALU :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
                      -> Grid a (Cin, (Opt1Bit, (Input,  (Input,  (Input,  (Input)))))) 
                                                (Output, (Output, (Output, (Output, (Cout)))))
a4BitALU aALU
    = eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next aALU))
    where next = second



a8BitALU :: Arrow a => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
                    -> Grid a (Cin, (Opt1Bit, (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  Input))))))))) 
                                              (Output, (Output, (Output, (Output, (Output, (Output, (Output, (Output, Cout))))))))
a8BitALU aALU
    = eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next aALU))))))
    where next = second




testALU4Op1Bit = a4Op1BitALU (aFullAdd) (noCarry aOr) (noCarry aXor) (noCarry $ aFst >>> aNot)
opFullAdd = (False, False)
opXOR     = (False, True)
opOR      = (True,  False)
opNOT     = (True,  True)


a4Op1BitALU :: (Arrow a)
          => Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Opt2Bit, (Cin, Input)) (Cout, Output)
a4Op1BitALU op01 op02 op03 op04
    =   second (   (op01 &&& op02) &&& (op03 &&& op04)
               >>> aDassoc     *** aDassoc
               >>> aDassoc
               )
    >>> aAssoc
    >>> a2Mux *** a2Mux

a4Op2BitALU :: (Arrow a)
            => Grid a (Opt2Bit, (Cin, Input)) (Cout, Output)
            -> Grid a (Opt2Bit, (Cin, In2Bit)) (Cout, Out2Bit)
a4Op2BitALU aBitALU
    =   eval >>> second (aBitALU >>> aFlip)
    where eval =   second a_aBC2ABc
               >>> aAssoc
               >>> first aBitALU
               >>> aFlip *** aFlip
               >>> a_ABc2aBC
               >>> second 
                   (   a_aBC2ABc
                   >>> aFlip
                   )

--    =   second a_aBC2ABc  
--    >>> aAssoc
--    >>> first (aBitALU >>> aFlip)
--    >>> a_ABc2aBC
--    >>> second 
--        (   a_aBC2ABc
--        >>> aFlip
--        >>> a_ABc2aBC
--        >>> aBitALU 
--        )

a4Op4BitALU :: (Arrow a)
          => Grid a (Opt2Bit, (Cin, Input)) (Cout, Output) 
          -> Grid a (Opt2Bit, (Cin, In4Bit)) (Cout, Out4Bit)
a4Op4BitALU aBitALU
    =   eval
    >>> second 
        (   eval
        >>> second
            (   eval
            >>> second (aBitALU >>> aFlip)
            )
        )
    >>> (aSnd >>> aSnd >>> aSnd >>> aSnd) &&& (second (second (second aFst)))
    where eval =   second a_aBC2ABc
               >>> aAssoc
               >>> first aBitALU
               >>> aFlip *** aFlip
               >>> a_ABc2aBC
               >>> second 
                   (   a_aBC2ABc
                   >>> aFlip
                   )

a4Op8BitALU :: (Arrow a)
          => Grid a (Opt2Bit, (Cin, Input)) (Cout, Output) 
          -> Grid a (Opt2Bit, (Cin, In8Bit)) (Cout, Out8Bit)
a4Op8BitALU aBitALU
    =   eval
    >>> second
        (   eval
        >>> second
            (   eval
            >>> second
                (   eval
                >>> second
                    (   eval
                    >>> second 
                        (   eval
                        >>> second
                            (   eval
                            >>> second (aBitALU >>> aFlip)
                            )
                        )
                    )
                )
            )
        )
    >>> (aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd >>> aSnd) &&& (second (second (second (second (second (second (second aFst)))))))
    where eval =   second a_aBC2ABc
               >>> aAssoc
               >>> first aBitALU
               >>> aFlip *** aFlip
               >>> a_ABc2aBC
               >>> second 
                   (   a_aBC2ABc
                   >>> aFlip
                   )

t8b :: Int -> (Bool,  (Bool,  (Bool,  (Bool, (Bool,  (Bool,  (Bool,  (Bool))))))))
t8b x 
    = (bit7, (bit6, (bit5, (bit4, (bit3, (bit2, (bit1, (bit0))))))))
    where (bit7: bit6: bit5: bit4: bit3: bit2: bit1: bit0: []) = i2t8b [] x

i2t8b :: [Bool] -> Int -> [Bool]
i2t8b list x 
    | x == 0
    = replicate (8 - (length list)) False ++ list

    | x == 1
    = replicate (8 - (length list) - 1) False ++ (True : list)

    | x `mod` 2  == 0  
    = i2t8b (False : list) (x `div` 2)

    | x `mod` 2 == 1
    = i2t8b (True : list) ( (x-1) `div` 2)

inp8b :: Int -> Int -> In8Bit
inp8b x1 x2
    = ((bit07, bit17), ((bit06, bit16), ((bit05, bit15), ((bit04, bit14), ((bit03, bit13), ((bit02, bit12), ((bit01, bit11), ((bit00, bit10)))))))))
    where (bit07: bit06: bit05: bit04: bit03: bit02: bit01: bit00: []) = i2t8b [] x1
          (bit17: bit16: bit15: bit14: bit13: bit12: bit11: bit10: []) = i2t8b [] x2




tAnd 
    = [ (False, False)    --  [ False
      , (False, True )    --  , False
      , (True , False)    --  , False
      , (True , True )    --  , False
      ]                   --  ]
       
tFullAdd
    = [ (False, (False, False))   --	[ (False, False)
      , (False, (False, True ))   --	, (False, True)
      , (False, (True , False))   --	, (False, True)
      , (False, (True , True ))   --	, (True,  False)
                               
      , (True , (False, False))   --	, (False, True)
      , (True , (False, True ))   --	, (True,  False)
      , (True , (True , False))   --	, (True,  False)
      , (True , (True , True ))   --	, (True,  True)
      ]                           --    ]
      
t1BitALU
    = [ ((False, False), (False, False))	--  [ (False, False)	[ (False, False)
      , ((False, False), (False, True ))	--  , (False, True) 	, (False, True)
      , ((False, False), (True , False))	--  , (False, True) 	, (False, True)
      , ((False, False), (True , True ))	--  , (True,  True) 	, (True,  False)
                                                                
      , ((True , False), (False, False))	--  , (False, True) 	, (False, True)
      , ((True , False), (False, True ))	--  , (True,  False)	, (True,  False)
      , ((True , False), (True , False))	--  , (True,  False)	, (True,  False)
      , ((True , False), (True , True ))	--  , (True,  True) 	, (True,  True)
                                                                    
      , ((False, True),  (False, False))	--  , (False, False)	, (False, False)
      , ((False, True),  (False, True ))	--  , (False, True) 	, (False, False)
      , ((False, True),  (True , False))	--  , (False, True) 	, (False, False)
      , ((False, True),  (True , True ))	--  , (True,  True) 	, (True,  True)
                                                                
      , ((True , True),  (False, False))	--  , (False, True) 	, (False, False)
      , ((True , True),  (False, True ))	--  , (True,  False)	, (True,  False)
      , ((True , True),  (True , False))	--  , (True,  False)	, (True,  False)
      , ((True , True),  (True , True ))	--  , (True,  True) 	, (True,  True)
      ]                                 	--  ]                   ]


t2HullALU                                   --  runStream (simulate $ a2HullALU aFullAdd (carry_connect_through aAnd)) $ t2HullALU
    = [ (False, (False, (False, False)))	--	[ (False, False)
      , (False, (False, (False, True )))	--	, (False, True)
      , (False, (False, (True , False)))	--	, (False, True)
      , (False, (False, (True , True )))	--	, (True,  False)
                                                                
      , (False, (True , (False, False)))	--	, (False, True)
      , (False, (True , (False, True )))	--	, (True,  False)
      , (False, (True , (True , False)))	--	, (True,  False)
      , (False, (True , (True , True )))	--	, (True,  True)
                                                                
      , (True , (False, (False, False)))	--	, (False, False)
      , (True , (False, (False, True )))	--	, (False, False)
      , (True , (False, (True , False)))	--	, (False, False)
      , (True , (False, (True , True )))	--	, (False, True)
                                                                
      , (True , (True,  (False, False)))	--	, (False, False)
      , (True , (True,  (False, True )))	--	, (False, False)
      , (True , (True,  (True , False)))	--	, (False, False)
      , (True , (True,  (True , True )))	--	, (False, True)]
      ]                                

tTestALU4Op1Bit 
    = [ ( (True, False) , (False, (False, False)) )
      , ( (True, False) , (False, (False, True)) )
      , ( (True, False) , (False, (True,  False)) )
      , ( (True, False) , (False, (True,  True)) )
      ]
--      , ( (False, False) , (False, (,)) )
--      , ( (False, False) , (False, (,)) )
--      , ( (False, False) , (False, (,)) )
--      , ( (False, False) , (False, (,)) )
