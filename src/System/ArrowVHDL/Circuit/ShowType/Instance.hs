{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}


-- Das Modul \hsSource{Circuit.ShowType.Instance} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise
-- beschreiben, bearbeiten oder benutzen zu können.

module System.ArrowVHDL.Circuit.ShowType.Instance
where


-- Folgenden Module werden benötigt, um die Instanzen definieren zu können:

import System.ArrowVHDL.Circuit.ShowType.Class

import Prelude hiding (id, (.))
import qualified Prelude as Pr

import Control.Category 

import System.ArrowVHDL.Circuit.Graphs
import System.ArrowVHDL.Circuit.Descriptor

-- Showtype wird hier für bestimmte Tupel-Typen beschrieben. Einzig die Tupel-Information ist das, was mittels Showtype in Pin-Informationen
-- übersetzt wird. 


-- Die \hsSource{ShowType}-Instanz definiert die verschiedenen Typenvarianten, welche abbildbar sind.

-- Mit dem Typ \hsSource{(b, c) -> (c, b)} stellt die folgende Variante eine dar, in der die Ein- und Ausgänge miteinander vertauscht werden.

instance ShowType (b, c) (c, b) where
    showType _ 
        = emptyCircuit { nodeDesc = MkNode
                           { label   = "|b,c>c,b|" 
                           , nodeId  = 0
                           , sinks   = mkPins 2
                           , sources = mkPins 2
                           }
                     }
        where nd = nodeDesc emptyCircuit


-- Diese Variante mit dem Typ \hsSource{b -> (b, b)} beschreibt den Fall, in dem ein Eingang verdoppelt wird.
  
instance ShowType b (b, b) where
    showType _ 
        = emptyCircuit { nodeDesc = MkNode
                           { label   = "|b>b,b|" 
                           , nodeId  = 0
                           , sinks   = mkPins 1
                           , sources = mkPins 2
                           }
                     }
        where nd = nodeDesc emptyCircuit
  

-- Mit dem Typ \hsSource{(b, b) -> b} wir dann der Fall abgebildet, der zwei Eingänge auf einen zusammenfasst. 

instance ShowType (b, b) b where
    showType _ 
        = emptyCircuit { nodeDesc = MkNode
                           { label   = "|b,b>b|"
                           , nodeId  = 0
                           , sinks   = mkPins 2
                           , sources = mkPins 1
                           }
                     }
        where nd = nodeDesc emptyCircuit
  
-- instance ShowType (b, c) (b', c') where
--   showType _ = emptyCircuit { label = "b,c>b',c'"
--                           , sinks = mkPins 1
--                           , sources = mkPins 1
--                           }
--   
-- instance ShowType b b where
--  showType _ = emptyCircuit { label = "|b>b|"
--                          , sinks = mkPins 1
--                          , sources = mkPins 1
--                          }
--   
-- instance ShowType (b -> (c, d)) where
--   showType _ = emptyCircuit { label = "b>c,d"
--                           , sinks = mkPins 1
--                           , sources = mkPins 1
--                           }
-- 
-- instance ShowType ((b, c) -> d) where
--   showType _ = emptyCircuit { label = "b,c>d"
--                           , sinks = mkPins 1
--                           , sources = mkPins 1
--                           }
  
-- Letztlich bleibt noch der allgemeinste Fall der Möglich ist. Diese Varianten ist somit auch eine \begriff{CatchAll} Variante.

instance ShowType b c where
    showType _ 
        = emptyCircuit { nodeDesc = MkNode
                           { label   = "|b>c|"
                           , nodeId  = 0
                           , sinks   = mkPins 1
                           , sources = mkPins 1
                           }
                     }
        where nd = nodeDesc emptyCircuit
