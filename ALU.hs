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

-- |'aXum' is the Multiplexer where the last input-pin is the s-line
aXum =   aFlip 
     >>> aMux


a1Bit_MuxOp :: (Arrow a) => Grid a (Bool, (Bool, Bool)) (Bool)
a1Bit_MuxOp 
    =   second (   aDup
               >>> aOr *** aAnd
               )
    >>> aMux

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


--                               Cin     a     b      Cout   e
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
    where _aFlipBrc = aFlip >>> aSwapSnd >>> first aFlip


--                                Cin    Opt       a     b      Cout   e
a1BitALU :: (Arrow a) => Grid a ((Bool, (Bool)), (Bool, Bool)) (Bool, Bool)
a1BitALU 
    =   aSwapSnd
    >>> first 
        (   aFullAdd &&& (aSnd >>> aAnd)
        >>> a_ABc2aBC
        >>> aFlip
        )
    >>> aSwapSnd
    >>> aFlip
    >>> second
        (   aFlip
        >>> aMux
        )

type Input   = (Bool, Bool)
type Output  = Bool
type Cin     = Bool
type Cout    = Bool
type Opt1Bit = Bool


aDassoc :: (Arrow a) => Grid a ((b, c), (d, e)) ((b, d), (c, e))
aDassoc 
    =   aSwapSnd
    >>> a_aBC2ABc *** aId
    >>> a_ABc2aBC
    >>> aId *** aFlip


carry_connect_through :: (Arrow a) => (Grid a Input Output) -> Grid a (Cin, Input) (Cout, Output)
carry_connect_through arrow = (aConst False) *** arrow


a2HullALU :: (Arrow a) 
          => Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Cin, Input) (Cout, Output) 
          -> Grid a (Opt1Bit, (Cin, Input)) (Cout, Output)
a2HullALU h1 h2
    =   second (h1 &&& h2 >>> aDassoc)
    >>> aAssoc
    >>> aMux *** aMux






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


t2HullALU
    = [ (False, (False, (False, False)))	--	[ (False, False)
      , (False, (False, (False, True )))	--	, (False, True)
      , (False, (False, (True , False)))	--	, (False, True)
      , (False, (False, (True , True )))	--	, (True,  False)
                                        
      , (False, (True , (False, False)))	--	, (False, True)
      , (False, (True , (False, True )))	--	, (True,  False)
      , (False, (True , (True , False)))	--	, (True,  False)
      , (False, (True , (True , True )))	--	, (True,  True)
                                        
      , (True , (False, (False, False)))	--	, (False, False)
      , (True , (False, (False, True )))	--	, (False, True)
      , (True , (False, (True , False)))	--	, (False, True)
      , (True , (False, (True , True )))	--	, (True,  False)
                                        
      , (True , (True,  (False, False)))	--	, (False, True)
      , (True , (True,  (False, True )))	--	, (True,  False)
      , (True , (True,  (True , False)))	--	, (True,  False)
      , (True , (True,  (True , True )))	--	, (True,  True)
      ]                                

    --  runStream (simulate $ a2HullALU aFullAdd aFullAdd) $ t2HullALU
	--	[ (False, False)
	--	, (False, True)
	--	, (False, True)
	--	, (True,  False)

	--	, (False, True)
	--	, (True,  False)
	--	, (True,  False)
	--	, (True,  True)

	--	, (False, False)
	--	, (False, True)
	--	, (False, True)
	--	, (True,  False)

	--	, (False, True)
	--	, (True,  False)
	--	, (True,  False)
	--	, (True,  True)
	--	]


    --  runStream (simulate $ a2HullALU aFullAdd (carry_connect_through aAnd)) $ t2HullALU
	--	[ (False, False)
	--	, (False, True)
	--	, (False, True)
	--	, (True,  False)

	--	, (False, True)
	--	, (True,  False)
	--	, (True,  False)
	--	, (True,  True)

	--	, (False, False)
	--	, (False, False)
	--	, (False, False)
	--	, (False, True)

	--	, (False, False)
	--	, (False, False)
	--	, (False, False)
	--	, (False, True)]
