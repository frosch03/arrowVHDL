{-# LANGUAGE Arrows #-}
module NEWTEST
where

import Control.Arrow
import Traversal

plus :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
plus  = saugmentStructFunct def f
    where def = emptyStructure { name = "PLUS" }
          f   = uncurry (+)

minus :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
minus  = saugmentStructFunct def f 
    where def = emptyStructure { name = "MINUS" }
          f   = uncurry (-)

mal :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
mal  = saugmentStructFunct def f 
    where def = emptyStructure { name = "MAL" }
          f   = uncurry (*)

-- mal :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
-- mal  = saugmentStructFunct def f 
--     where def = emptyStructure { name = "MAL" }
--           f   = uncurry (*)
-- 
-- mal :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
-- mal  = saugmentStructFunct def f 
--     where def = emptyStructure { name = "MAL" }
--           f   = uncurry (*)
-- 
-- mal :: (Arrow a) => TraversalArrow a (Int, Int) (Int)
-- mal  = saugmentStructFunct def f 
--     where def = emptyStructure { name = "MAL" }
--           f   = uncurry (*)


toCaC :: (Arrow a) => TraversalArrow a Int Int
toCaC  = saugmentSubcomponent def f 
    where def = emptyStructure { name = "TOcaC" }
          f   = proc (x) -> do
                    tmp <- plus  -< (x, 32)
                    erg <- mal   -< (tmp, 1)
                    returnA      -< erg


toCaC2 :: (Arrow a) => TraversalArrow a Int Int
toCaC2  = saugmentSubcomponent def f 
    where def = emptyStructure { name = "TOcaC2" }
          f   =   arr (\x -> ((x, 32), 1))
              >>> first plus 
              >>> mal

toCaC3 :: (Arrow a) => TraversalArrow a Int Int 
toCaC3 = saugmentSubcomponent def f
  where def = emptyStructure{name = "TOcaC"}
        f =     arr (\(x) -> (x, 32)) 
            >>>    plus 
                >>> arr (\tmp -> (tmp, 1)) 
                >>> arr id
                   
            >>> mal 
            

