-- Das Modul \hsSource{Circuit.Arrow.Helpers} beschreibt Hilfsfunktionen, zur Arrow-Gestaltung.


module System.ArrowVHDL.Circuit.Arrow.Helpers
where



-- Folgenden Module werden benötigt, um die Arrows definieren zu können:


import Control.Category

import System.ArrowVHDL.Circuit.Arrow
import System.ArrowVHDL.Circuit.Descriptor
import System.ArrowVHDL.Circuit.Grid
import System.ArrowVHDL.Circuit.Stream
import System.ArrowVHDL.Circuit.Graphs 




-- Weitere Hilfsfunktionen werden notwendig, um schon bestehende \hsSource{Grid}-Arrows mit Schaltkreis Beschreibungen anzureichern. 


insert :: b -> (a, b) -> (a, b)
insert sg ~(x, _) = (x, sg)

insEmpty = insert emptyCircuit { nodeDesc = nodedesc }
    where nodedesc = MkNode { label = "eeeempty", nodeId = 0, sinks = mkPins 1, sources = mkPins 3 }

augment :: (Arrow a) => CircuitDescriptor -> a b c -> Grid a b c
augment sg f = GR (f, sg)




-- Zu guter letzt werden noch Funktionen benötigt, die bei der Umstrukturierung von Daten gebraucht werden.


movebrc :: ((a, b), c) -> (a, (b, c))
movebrc ~(~(x, y), sg) = (x, (y, sg))

backbrc :: (a, (b, c)) -> ((a, b), c)
backbrc ~(x, ~(y, sg)) = ((x, y), sg)

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), sg) = ((x, sg), y)
