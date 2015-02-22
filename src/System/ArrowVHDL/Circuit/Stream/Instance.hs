{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}

-- Das Modul \hsSource{Circuit.Stream.Instance} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
-- bearbeiten oder benutzen zu können.

module System.ArrowVHDL.Circuit.Stream.Instance
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import Prelude hiding (id, (.))
import qualified Prelude as Pr

import Control.Category 

import System.ArrowVHDL.Circuit.Arrow

import System.ArrowVHDL.Circuit.Descriptor
import System.ArrowVHDL.Circuit.Graphs
import System.ArrowVHDL.Circuit.Workers (flatten)

import System.ArrowVHDL.Circuit.Stream.Datatype



-- Im nächsten Schritt wird \hsSource{Stream} dann zu einer Kategorie ernannt, indem die \hsSource{Category}-Typklasse für \hsSource{Stream}
-- implementiert wird. Erst wenn \hsSource{Stream} eine Kategorie ist, lässt sich \hsSource{Stream} in einen Arrow befördern.

instance Category Stream where
    id              
      = SF (id)

    (SF f) . (SF g) 
      = SF (f . g) 


-- Nachdem \hsSource{Stream} eine Kategorie ist, kann \hsSource{Stream} als Arrow implementiert werden. Ähnlich wie bei der Implementierung der
-- Arrow Instanz von \hsSource{Grid} ist es auch bei \hsSource{Stream} notwendig, alle Funktionsbeschreibungen anzugeben. Die abgeleiteten
-- Funktionen der minimal Definition, reichen nicht aus. 

instance Arrow Stream where
    arr f             
      = SF $ map f -- ?? (SF . map) f ??

    first  (SF f)     
      = SF $ (uncurry zip) . (\(bs, cs) -> (f bs, cs)) . unzip 

    second (SF g)     
      = SF $ (uncurry zip) . (\(bs, cs) -> (bs,  g cs)) . unzip 

    (SF f) *** (SF g) 
      = SF $ (uncurry zip) . (\(bs, cs) -> (f bs, g cs)) . unzip


-- Da \hsSource{Stream} ein Arrow ist und Kontinuierliche Datenströme für das Looping notwendig sind, kann für \hsSource{Stream} auch die
-- \hsSource{ArrowLoop} Instanz angegeben werden.

instance ArrowLoop Stream where
    loop (SF f) 
      = SF $ (\bs -> 
               let (cs, ds) = unzip . f $ zip bs (stream ds) 
               in  cs
             )
      where stream ~(x:xs) = x:stream xs


-- Mit der \hsSource{Stream} Instanz von \hsSource{ArrowLoop} ist es nun auch möglich, die \hsSource{ArrowCircuit} Instanz zu implementieren.
-- Diese ist eine direkte Umsetzung des \hsSource{delay}'s auf die Listenfunktionalität. 

instance ArrowCircuit Stream where
    delay x = SF (x:)
