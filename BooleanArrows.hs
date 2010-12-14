module BooleanArrows
    ( aAnd
    , aOr
    , aNot
    )
where

import Control.Arrow 

import Traversal

aAnd :: (Arrow a) => TraversalArrow a (Bool, Bool) Bool
aAnd =  saugmentStructFunct def (uncurry (&&))
    where def = Annotate { name         = "AND"
                         , returnvalue  = Nothing
                         , formatstring = "%o1 <= %i1 and %i2;"
                         , inputs       = [ "first"
                                          , "second"
                                          ]   
                         , output       = "ret"
                         , predecessor  = Left []
                         }   

aOr  :: (Arrow a) => TraversalArrow a (Bool, Bool) Bool
aOr  =  saugmentStructFunct def (uncurry (||))
    where def = Annotate { name         = "OR"
                         , returnvalue  = Nothing
                         , formatstring = "%o1 <= %i1 or  %i2;"
                         , inputs       = [ "first"
                                          , "second"
                                          ]   
                         , output       = "ret"
                         , predecessor  = Left []
                         }   

aNot :: (Arrow a) => TraversalArrow a Bool Bool
aNot =  saugmentStructFunct def (not)
    where def = Annotate { name         = "NOT"
                         , returnvalue  = Nothing
                         , formatstring = "%o1 <= not %i1;"
                         , inputs       = [ "param" ]
                         , output       = "ret"
                         , predecessor  = Left []
                         }   

aDup :: (Arrow a) => TraversalArrow a Bool (Bool, Bool)
aDup =  saugmentStructFunct def ((\x -> (x,x)))
    where def = Annotate { name         = "DUP"
                         , returnvalue  = Nothing
                         , formatstring = "%o1, %o2 <= %i1"
                         , inputs       = [ "single" ]
                         , output       = "double"
                         , predecessor  = Left []
                         }

aDrp :: (Arrow a) => TraversalArrow a (Bool, Bool) Bool
aDrp =  saugmentStructFunct def ((\(x,_) -> x))
    where def = Annotate { name         = "DRP"
                         , returnvalue  = Nothing
                         , formatstring = "%o2 <= %i1, %i2"
                         , inputs       = [ "first", "droped" ]
                         , output       = "single"
                         , predecessor  = Left []
                         }

aId :: (Arrow a) => TraversalArrow a b b
aId =  saugmentStructFunct def (id)
    where def = Annotate { name         = "ID"
                         , returnvalue  = Nothing
                         , formatstring = "%o1 <= %i1"
                         , inputs       = [ "in" ]
                         , output       = "out"
                         , predecessor  = Left []
                         }
