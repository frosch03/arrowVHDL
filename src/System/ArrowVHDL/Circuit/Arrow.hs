{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}

module System.ArrowVHDL.Circuit.Arrow 
  ( Arrow(..)
  , ArrowLoop(..)
  , ArrowCircuit(..)
  , ArrowChoice(..)
  , returnA
  , movebrc
  , backbrc
  , swapsnd
  ) 
where


import Prelude (id)

import System.ArrowVHDL.Circuit.Arrow.Class
import System.ArrowVHDL.Circuit.Arrow.Instance



-- | 'returnA' is a standard arrow-function that is similar to return in the monad context
returnA :: (Arrow a) => a b b
returnA = arr id




-- | 'movebrc', 'backbrc' and 'swapsnd' are functions that change the order of tuples
movebrc :: ((a, b), c) -> (a, (b, c))
movebrc ~(~(x, y), sg) = (x, (y, sg))

backbrc :: (a, (b, c)) -> ((a, b), c)
backbrc ~(x, ~(y, sg)) = ((x, y), sg)

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), sg) = ((x, sg), y)
