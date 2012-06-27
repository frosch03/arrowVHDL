-- Das Modul \hsSource{Circuit.Stream.Datatype} beschreibt, was ein Stream-Datentyp ist.

module Circuit.Stream.Datatype
where


-- In Schaltkreisen sind Schleifen nicht über den normalen \hsSource{ArrowLoop} Ansatz realisierbar. Das Problem liegt darin, dass zwischen
-- zwei Berechnungen keine Verzögerung stattfindet. Wollte man das erreichen, so benötigt man mindestens \begriff{Register}, die einen
-- Taktzyklus verzögern. Damit ist dann festgelegt, dass im Grunde Schleifen in Hardware nur dann Sinn ergeben, wenn ein kontinuierlicher
-- Datenstrom verarbeitet werden kann. 

-- Auch für andere Ansätze wird eine \hsSource{Stream}-Arrow notwendig. Dies kann beispielsweise der Fall sein falls Zwischenergebnisse
-- Innerhalb des Schaltkreises ermittelt werden sollen. 

-- Zunächst wird der Datentyp definiert. \hsSource{Stream} hat einen Typkonstruktor \hsSource{SF} und besteht aus einer Funktion, welche aus
-- einer Liste von b's (\hsSource{[b]}) eine Liste von c's \hsSource{[c]}) erzeugt. 

newtype Stream b c = SF { runStream :: ([b] -> [c]) }
