{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, OverlappingInstances, RankNTypes, ExistentialQuantification #-}
module Test where

import Control.Category
import Control.Arrow

import Data.Binary
import qualified Data.ByteString

import Prelude hiding (id, (.))
import qualified Prelude as Pr


newtype NamedArrow a b c = NAr (a b c, String) 


instance (Category a) => Category (NamedArrow a) where
    id  = id
    (.) (NAr (f, sf)) (NAr (g, sg))
        = NAr $ (f . g, sg ++ " dann " ++ sf)

instance Arrow (NamedArrow (->)) where
    arr f              = NAr $ (f, "")
    first (NAr (f, _)) = arr (\(x, y) -> (f x, y))

----------------------



newtype Stream b c = SF { runSF :: [b] -> [c] }


instance Category (Stream) where
    id                = SF id
    (.) (SF f) (SF g) = SF (f . g)

instance Arrow (Stream) where
    arr   f      = SF (map f)
    first (SF f) = SF $   unzip 
                      >>> (\(bs, cs) -> (f bs, cs))
                      >>> uncurry zip
----------------------



data Measure     = forall a . M a
type Measurement = [Measure]


newtype Sensor b c = Sens {sense :: (b, Measurement) -> (c, Measurement) }

instance Category (Sensor) where
    id    = Sens $ \(x, msmts) -> (x, (M x):msmts)
    f . g = Sens $ \(x, msmts) -> let (xg,  msmts_g) = sense g $ (x,  msmts)
                                      (xgf, msmts')  = sense f $ (xg, msmts_g)
                                  in  (xgf, msmts')

instance Arrow (Sensor) where
    arr f          = Sens $ \(x, msmts) -> let y = (f x)
                                           in  (y, (M y):msmts)
    first (Sens f) = Sens $   arr swapsnd 
                          >>> first f
                          >>> arr swapsnd

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), z) = ((x, z), y)

-- instance Show (Measurement) where
--     show []     = "" 
--     show (M x:xs) = (show x) ++ (show xs)
----------------------



instance Category (NamedArrow Stream) where 
    id  = id
    (.) (NAr (f, sf)) (NAr (g, sg))
        = NAr $ (f . g, sg ++ " / " ++ sf)
----------------------



run :: NamedArrow (->) b c -> (b -> c)
run (NAr (a, s)) = a

description :: NamedArrow (->) b c -> String
description (NAr (_, s)) = s 

dearr :: (Arrow a) => NamedArrow (a) b c -> a b c
dearr (NAr (a, _)) = a

simulate :: NamedArrow Stream b c -> ([b] -> [c])
simulate (NAr (a, _)) = runSF a

synthesize = description

measuring :: NamedArrow Sensor b c -> b -> Measurement
measuring (NAr (a, _)) = \x -> snd $ sense a (x, [M ()])
----------------------



aFunAdd :: (Arrow a) => a (Int, Int) Int
aFunAdd = arr $ uncurry (+)

aFunDup :: (Arrow a) => a b (b, b)
aFunDup = arr $ (\x -> (x, x))


naAdd :: (Arrow a) => NamedArrow a (Int, Int) Int
naAdd = NAr (aFunAdd, "addiere")

naDup :: (Arrow a) => NamedArrow a Int (Int, Int)
naDup = NAr (aFunDup, "dupliziere")

naTimes2 = naDup >>> naAdd

naTest = naTimes2 >>> naTimes2
