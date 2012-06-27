{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}

module Circuit.Arrow 
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

import Circuit.Arrow.Class
import Circuit.Arrow.Instance



-- Das Modul Circuit.Arrow beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
-- bearbeiten oder benutzen zu können.

-- Neben den Klassendefinitionen ist es ganz Praktisch, einen Arrow zu definieren, der in allen Arrow-Beschreibungen wieder Anwendung finden
-- wird. Es handelt sich um 'returnA', was der Kategorientheoretischen _Unit_ Funktion entspricht.


returnA :: (Arrow a) => a b b
returnA = arr id



-- Zu guter letzt werden noch Funktionen benötigt, die bei der Umstrukturierung von Daten gebraucht werden.


movebrc :: ((a, b), c) -> (a, (b, c))
movebrc ~(~(x, y), sg) = (x, (y, sg))

backbrc :: (a, (b, c)) -> ((a, b), c)
backbrc ~(x, ~(y, sg)) = ((x, y), sg)

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), sg) = ((x, sg), y)
