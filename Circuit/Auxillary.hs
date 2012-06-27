-- Das Hilfsmodul stellt ein Sammelbecken für alle Funktionen dar, die nicht in eines der anderen Module gepasst haben.


  module Circuit.Auxillary 
  where




-- Da dieses Modul keine spezielle Aufgabe besitzt sind auch die Aufgaben der einzelnen Funktionen sehr unterschiedlich. Aus diesen Gründen ist
-- die Liste der eingebundenen Module recht lang. 


import Data.List (union, groupBy, isInfixOf)
import Data.Maybe
import Data.Either
import Control.Monad (msum)

import GHC.Exts (sortWith)

import Circuit.Grid
import Circuit.Stream

import Control.Category
import Circuit.Arrow

import Circuit.Grid
import Circuit.Stream

import Circuit.Descriptor
import Circuit.Show
import Circuit.Tests
import Circuit.Splice
import Circuit.Sensors
import Circuit.Workers

import Circuit.Graphs (emptyCircuit)



-- Mit \hsSource{nextID} hat man eine Funktion, die eine Liste vom Komponenten Nummer erhält und daraus dann eine nächste gültige Komponenten
-- Nummer erzeugt.


nextID :: [CompID] -> CompID
nextID []    = 0
nextID [cid] = cid + 1
nextID cids  = nextID [foldl max 0 cids]



--  %%% Unter Ferner liefen XXX 
-- Die Funktion \hsSource{onlyInnerEdges} filtert aus einer Liste von Kanten genau diese Kanten heraus, die die internen Kanten im Schaltkreis
-- darstellen. Die Ergebnismenge enthält keine Ein- und Ausgehenden Kanten.


onlyInnerEdges :: [Edge] -> [Edge]
onlyInnerEdges es = es'
    where es' = filter notIO $ es
          notIO :: Edge -> Bool
          notIO (MkEdge (Nothing, _) _) = False
          notIO (MkEdge _ (Nothing, _)) = False
          notIO _                       = True



-- Typischerweise verwendet man den Begriff \begriff{Synthese} in der Hardware-Community für den Prozess, aus einer Modellhaften
-- Hardwarebeschreibung heraus tatsächlichen Hardwarecode (beispielsweise VHDL) zu erzeugen. Daneben ist auch die \begriff{Simulation} von
-- Hardwaremodellen notwendig, um die entworfenen Modelle vor der Realisierung überprüfen zu können.
-- 
-- 
-- Die beiden Prozesse lassen sich auch auf das \hsSource{Grid}-Arrow Modell übertragen. So stellt \hsSource{synthesize} eine Funktion dar, die
-- aus einem gegebenen \hsSource{Grid} die fertige Hardwarebeschreibung \footnote{in diesem Fall ausgeliefert in VHDL} ausliest. Die Simulation
-- wird mittels der Funktion \hsSource{simulate} abgebildet. Diese Funktion liest nun aus einem \hsSource{Grid} den Arrow heraus und überführt
-- diesen in einen \hsSource{Stream}-Arrow, der dann mit einem kontinuierlichem Datenstrom simuliert werden kann.


synthesize :: Grid (->) b c -> CircuitDescriptor
--synthesize (GR (_, cd)) = flatten cd   -- %%% TODO : Flatten won't work with Looping-Stuff ...
synthesize (GR (_, cd)) = cd

simulate :: Grid (->) b c -> Stream b c 
simulate f = arr (toFunctionModel f)



-- Um einen \hsSource{Grid}-Arrow kombinatorisch auszuwerten, existiert die Hilfsfunktion \hsSource{toFunctionModel}, die ein Synonym für
-- \hsSource{runGrid} ist.
  

toFunctionModel :: Grid (->) b c -> (b -> c)
toFunctionModel = runGrid

  

-- %%% TODO : Ist synthesize mit unit () möglich? 
-- %%% TODO : Keine zufälligen daten ... 
-- %%% TODO : Frage nach simulate / synthesize => right ... 
-- %%% TODO : _kritische_pfad_analyse_ / ... 



 
-- Weitere Hilfsfunktionen werden notwendig, um schon bestehende \hsSource{Grid}-Arrows mit Schaltkreis Beschreibungen anzureichern. 


insert :: b -> (a, b) -> (a, b)
insert sg ~(x, _) = (x, sg)

insEmpty = insert emptyCircuit { nodeDesc = MkNode { label = "eeeempty", nodeId = 0, sinks = mkPins 1, sources = mkPins 3 } }

augment :: (Arrow a) => CircuitDescriptor -> a b c -> Grid a b c
augment cd_f f = GR (f, cd_f)

