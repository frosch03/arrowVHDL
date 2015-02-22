module System.ArrowVHDL.Test where

import Prelude

import Control.Category ((>>>))

import System.ArrowVHDL.Circuit
import System.ArrowVHDL.Circuit.Arrow
import System.ArrowVHDL.Circuit.IEEE_STD_LOGIC_1164

aBlub :: (Arrow a) => Grid a (Bool, Bool) Bool
aBlub = aXor >>> aNot
