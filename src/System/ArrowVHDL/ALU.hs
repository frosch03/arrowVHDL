{-# LANGUAGE Arrows, NoMonomorphismRestriction, RebindableSyntax #-}
module System.ArrowVHDL.ALU 
where

import Prelude hiding (id, (.))

import Control.Category -- here we get >>> ...

import System.ArrowVHDL.Circuit
import System.ArrowVHDL.Circuit.Arrow -- here we get first and second
import System.ArrowVHDL.Circuit.Defaults

type Input   = (Bool, Bool)
type Output  = Bool
type Cin     = Bool
type Cout    = Bool
type Opt1Bit = Bool
type Opt2Bit = (Opt1Bit, Opt1Bit)
type Opt3Bit = (Opt1Bit, Opt2Bit)
type Opt4Bit = (Opt1Bit, Opt3Bit)

type In2Bit  = (Input,  (Input))
type Out2Bit = (Output, (Output))

type In4Bit  = (Input,  (Input,  (Input,  (Input))))
type Out4Bit = (Output, (Output, (Output, (Output))))

type In8Bit  = (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  (Input))))))))
type Out8Bit = (Output, (Output, (Output, (Output, (Output, (Output, (Output, (Output)))))))) 


-- aFst
-- aSnd
-- aXor
-- aShiftL
-- aShiftR
-- aAdd


aDdistr_new :: (Arrow a) => Grid a ((b, c), (b, e)) (b, (c, e))
aDdistr_new 
    = proc ((x1, y), (x2, z)) -> do
        returnA -< (x1, (y, z))


-- |With the 'noC' operator, one can shortwire the CarryIn bit direct to the CarryOut bit
noC :: (Arrow a) => (Grid a Input Output) -> Grid a (Cin, Input) (Output, Cout)
noC arrow 
    =   second arrow
    >>> aFlip
    

-- |'aFullAdd' is the full adder implementation
aFullAdd :: (Arrow a) => Grid a (Cin, Input) (Output, Cout)
aFullAdd
    =   second (aXor &&& aAnd)
    >>> a_aBC2ABc
    >>> first  (aXor &&& aAnd)
    >>> a_ABc2aBC
    >>> second (aOr)

-- Multiplexer --

-- | With 'aMux' a 1 Bit multiplexer is defined
aMux :: (Arrow a) => Grid a (Opt1Bit, (Output, Output))  Output
aMux 
    =   aDistr
    >>> first (first aNot)
    >>> aAnd *** aAnd
    >>> aOr

aMux2Bit :: (Arrow a) --        00      01        10      11
         => Grid a (Opt2Bit, ((Output, Output), (Output, Output)))  Output
aMux2Bit
    =   a_ABc2aBC
    >>> second aDistr
    >>> second (aMux *** aMux)
    >>> aMux

aMux3Bit :: (Arrow a) --         000     001       010     011         100     101       110     111
         => Grid a (Opt3Bit, (((Output, Output), (Output, Output)), ((Output, Output), (Output, Output))))  Output
aMux3Bit
    =   a_ABc2aBC
    >>> second aDistr
    >>> second (aMux2Bit *** aMux2Bit)
    >>> aMux

aMux4Bit :: (Arrow a) 
         => Grid a (Opt4Bit, ( (((Output, Output), (Output, Output)), ((Output, Output), (Output, Output)))
                             , (((Output, Output), (Output, Output)), ((Output, Output), (Output, Output)))))  Output
aMux4Bit 
    =   a_ABc2aBC
    >>> second aDistr
    >>> second (aMux3Bit *** aMux3Bit)
    >>> aMux

-- |'anXum' is the Multiplexer where the last input-pin is the s-line
-- |it is generated out of one of the mux'es
anXum mux =   aFlip 
          >>> mux

aXum  = anXum aMux
a2Xum = anXum aMux2Bit
a3Xum = anXum aMux3Bit
a4Xum = anXum aMux4Bit

--_____________--

--eval :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) -> Grid a (Cin, (Opt1Bit, (Input, rest))) (Output, (Cout, (Opt1Bit, rest)))
-- |The 'eval' function takes a single bit ALU and evaluates the first bit of a multiBit input
-- so with 'eval' one can define the steps of n-Bit ALU
eval aALU 
    =   second aDistr
    >>> a_aBC2ABc
    >>> first aALU
    >>> a_ABc2aBC

-- n-Bit ALU's --

--a1BitALU :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input))   (Output, Cout) 
--                      -> Grid a (Cin, (Opt1Bit, (Input))) (Output, (Cout))
mk1BitALU = id


--a2BitALU :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
--                   -> Grid a (Cin, (Opt1Bit, (Input,  (Input)))) 
--                                             (Output, (Output, (Cout)))
mk2BitALU aALU 
    = eval aALU >>> next aALU
    where next = second


--a4BitALU :: (Arrow a) => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
--                      -> Grid a (Cin, (Opt1Bit, (Input,  (Input,  (Input,  (Input)))))) 
--                                                (Output, (Output, (Output, (Output, (Cout)))))
mk4BitALU aALU
    = eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next aALU))
    where next = second


--a8BitALU :: Arrow a => Grid a (Cin, (Opt1Bit, Input)) (Output, Cout) 
--                    -> Grid a (Cin, (Opt1Bit, (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  (Input,  Input))))))))) 
--                                              (Output, (Output, (Output, (Output, (Output, (Output, (Output, (Output, Cout))))))))
mk8BitALU aALU
    = eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next 
    ( eval aALU >>> next aALU))))))
    where next = second


-- n-Bit ALU's --
--_____________--

-- n-Bit Operators --

aOpt1Bit :: (Arrow a)
         => Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, (Opt1Bit, Input)) (Output, Cout)
aOpt1Bit aOp0 aOp1
    =   a_aBC2ABc
    >>> first aFlip
    >>> a_ABc2aBC
    >>> second 
        (   aOp0 &&& aOp1
        >>> aDdistr
        )
    >>> aDistr
    >>> aMux *** aMux


aOpt2Bit :: (Arrow a)
         => Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, (Opt2Bit, Input)) (Output, Cout)
aOpt2Bit aOp00 aOp01 aOp10 aOp11
    =   a_aBC2ABc
    >>> first aFlip
    >>> a_ABc2aBC
    >>> second 
        (   (aOp00 &&& aOp01) &&& (aOp10 &&& aOp11)
        >>> aDdistr *** aDdistr
        >>> aDdistr
        )
    >>> aDistr
    >>> aMux2Bit *** aMux2Bit


aOpt3Bit :: (Arrow a)
         => Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, (Opt3Bit, Input)) (Output, Cout)
aOpt3Bit aOp000 aOp001 aOp010 aOp011 aOp100 aOp101 aOp110 aOp111
    =   a_aBC2ABc
    >>> first aFlip
    >>> a_ABc2aBC
    >>> second
        (   ((aOp000 &&& aOp001) &&& (aOp010 &&& aOp011)) &&& ((aOp100 &&& aOp101) &&& (aOp110 &&& aOp111))
        >>> (aDdistr *** aDdistr) *** (aDdistr *** aDdistr)
        >>> aDdistr *** aDdistr
        >>> aDdistr
        )
    >>> aDistr
    >>> aMux3Bit *** aMux3Bit



aOpt4Bit :: (Arrow a)
         => Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, Input) (Cout, Output)
         -> Grid a (Cin, (Opt4Bit, Input)) (Output, Cout)
aOpt4Bit aOp0000 aOp0001 aOp0010 aOp0011 aOp0100 aOp0101 aOp0110 aOp0111  aOp1000 aOp1001 aOp1010 aOp1011 aOp1100 aOp1101 aOp1110 aOp1111
    =   a_aBC2ABc
    >>> first aFlip
    >>> a_ABc2aBC
    >>> second
        (       (((aOp0000 &&& aOp0001) &&& (aOp0010 &&& aOp0011)) &&& ((aOp0100 &&& aOp0101) &&& (aOp0110 &&& aOp0111))) 
            &&& (((aOp1000 &&& aOp1001) &&& (aOp1010 &&& aOp1011)) &&& ((aOp1100 &&& aOp1101) &&& (aOp1110 &&& aOp1111)))
        >>> ((aDdistr *** aDdistr) *** (aDdistr *** aDdistr)) *** ((aDdistr *** aDdistr) *** (aDdistr *** aDdistr))
        >>> (aDdistr *** aDdistr) *** (aDdistr *** aDdistr)
        >>> aDdistr *** aDdistr
        >>> aDdistr
        )
    >>> aDistr
    >>> aMux4Bit *** aMux4Bit


-- n-Bit Operators --
--_________________--

aFAD_XOR_AND_OR = aOpt2Bit aFullAdd (noC aXor) (noC aAnd) (noC aOr)

-- Auxillary --

t8b :: Int -> (Bool,  (Bool,  (Bool,  (Bool, (Bool,  (Bool,  (Bool,  (Bool))))))))
t8b x 
    = (bit7, (bit6, (bit5, (bit4, (bit3, (bit2, (bit1, (bit0))))))))
    where (bit7: bit6: bit5: bit4: bit3: bit2: bit1: bit0: []) = i2t8b [] x

i2t8b :: [Bool] -> Int -> [Bool]
i2t8b list x 
    | x == 0
    = list ++ (replicate (8 - (length list)) False)

    | x == 1
    = (list ++ [True]) ++ (replicate (8 - (length list) - 1) False)

    | x `mod` 2  == 0  
    = i2t8b (list ++ [False]) (x `div` 2)

    | x `mod` 2 == 1
    = i2t8b (list ++ [True]) ( (x-1) `div` 2)

inp8b :: Int -> Int -> In8Bit
inp8b x1 x2
    = ((bit07, bit17), ((bit06, bit16), ((bit05, bit15), ((bit04, bit14), ((bit03, bit13), ((bit02, bit12), ((bit01, bit11), ((bit00, bit10)))))))))
    where (bit07: bit06: bit05: bit04: bit03: bit02: bit01: bit00: []) = i2t8b [] x1
          (bit17: bit16: bit15: bit14: bit13: bit12: bit11: bit10: []) = i2t8b [] x2

-- Auxillary --
--___________--
