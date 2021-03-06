-- Das Modul \hsSource{Circuit.Sensors} enthält eine Reihe von Funktionen, die bestimmte Werte aus übergebenen Daten herauslesen. 


module System.ArrowVHDL.Circuit.Sensors
where
 


-- Es werden lediglich die Standard Definitionen, sowie die Tests benötigt.


import System.ArrowVHDL.Circuit.Descriptor
import System.ArrowVHDL.Circuit.Tests
 


-- \subsection{Schaltungssensoren}
-- Die Funktion \hsSource{allCircuits} holt aus einer Schaltung all die Bausteine heraus, aus denen diese Schaltung aufgebaut ist. Das
-- Ergebnis wird dann als Liste von Bausteinen zurückgegeben. 


allCircuits :: CircuitDescriptor -> [CircuitDescriptor]
allCircuits sg 
    = if (length next_sg == 0) then sg : []
                               else sg : (concat $ map allCircuits next_sg)
    where next_sg = nodes sg
          cid     = nodeId.nodeDesc $ sg 




-- Mithilfe der Funktion \hsSource{maxCompID} lässt sich die maximale Komponentennummer einer Schaltung anzeigen.


maxCompID :: CircuitDescriptor -> CompID
maxCompID sg = (nodeId.nodeDesc $ sg) `max` (foldl max 0 $ map maxCompID (nodes sg))




-- Mit der Funktion \hsSource{getComp} lässt sich ein definierter Baustein aus einer Schaltung auslesen. Identifiziert wird der auszulesende
-- Bausteine über seine Komponentennummer. \hsSource{getComp} ist hier die Funktion mit Fehlerbehandlung, die Logik selber steckt in
-- \hsSource{getComp'}.


getComp :: CircuitDescriptor -> CompID -> CircuitDescriptor
getComp g cid = if length output == 1 
                then head output
                else error "getComp: there is no such circuit"
    where output = getComp' g cid

getComp' :: CircuitDescriptor -> CompID -> [CircuitDescriptor]
getComp' g cid 
    | (nodeId.nodeDesc $ g) == cid 
    = [g]
    | otherwise       
    = concat $ map (flip getComp' cid) (nodes g)




-- Um den übergeordneten Schaltkreis eines Bausteins zu erhalten, kann man sich der Funktion \hsSource{superNode} bedienen. Diese Funktion
-- erwartet eine Schaltung sowie eine Komponentennummer. Im Falle das es eine SuperNode \footnote{also eine Schaltung, die mindestens aus der
-- Komponente mit der übergebenen Nummer besteht} gibt, wird diese Zurückgeliefert. Andernfalls wird eine Fehlermeldung ausgegeben.


superNode :: CircuitDescriptor -> CompID -> CircuitDescriptor
superNode g cid 
    = if length output == 1
        then head output
        else error "superNode: there is no such supernode"
    where output = superNode' g cid

superNode' :: CircuitDescriptor -> CompID -> [CircuitDescriptor]
superNode' g cid 
    | g `isSuperNodeOf` cid
    = [g]
    | otherwise
    = concat $ map (flip superNode' cid) $ nodes g




-- Als atomar werden Schaltungen bezeichnet, die selbst aus keinen weiteren Schaltungen aufgebaut sind. Diese Schaltungen können auch
-- \begriff{Bausteine} genannt werden. Die Funktion \hsSource{nextAtomic} ermöglicht es, aus einer gegebenen Schaltung und einer Kante die
-- nächste Komponente zu ermitteln, die atomar ist. Außerdem wird auch noch der Pin mit angegeben, über welchen diese Komponente angeschlossen
-- ist.


nextAtomic :: CircuitDescriptor -> Edge -> (CompID, PinID)
nextAtomic g e
    | isToOuter e && (nodeId.nodeDesc $ super) == mainID
    = (mainID, snkPin e)
     
    | isToOuter e
    = nextAtomic g $ head $ filter (\x -> sourceInfo x == (Just $ nodeId.nodeDesc $ super, snkPin e)) $ edges supersuper

    | not.isAtomic $ sub 
    = nextAtomic g $ head $ filter (\x -> (isFromOuter x) && (snkPin e == srcPin x)) $ edges sub

    | isAtomic sub
    = (snkComp e, snkPin e)
    where mainID     = nodeId.nodeDesc $ g
          sub        = getComp   g (snkComp e)
          super      = superNode g (srcComp e)
          supersuper = superNode g (nodeId.nodeDesc $ super)



-- \subsection{Kantensesoren}
-- 
-- Mit der Funktion \hsSource{fromCompEdges} können alle Kanten ausgelesen werden, die von einer bestimmten Komponente her kommen. 


fromCompEdges :: CircuitDescriptor -> CompID -> [Edge]
fromCompEdges g cid
    = filter (\x -> (not.isFromOuter $ x) 
                 && (cid == (srcComp x) ) ) $ edges $ superNode g cid




-- Hilfreich ist auch die Funktion \hsSource{allEdges}, die wie der Name schon vermuten lässt, alle Kanten die innerhalb einer Schaltung
-- verbaut werden, sammelt und als Ergebnisliste zurückgibt. 


allEdges :: CircuitDescriptor -> [Edge]
allEdges g = edges g ++ (concat $ map allEdges (nodes g))




-- \begriff{snk} und \begriff{src} sind Kurzschreibweisen für \begriff{Sink} und \begriff{Source}. Mit den Funktionen \hsSource{snkPin} und
-- \hsSource{srcPin} lassen sich aus einer Kante entweder der Quell- oder der Zielpin auslesen. \hsSource{snkComp} und \hsSource{srcComp} sind
-- dann für die Quell- sowie für die Zielkomponente verantwortlich.


snkPin :: Edge -> PinID
snkPin (MkEdge (_, _) (_, pid)) = pid

srcPin :: Edge -> PinID
srcPin (MkEdge (_, pid) (_, _)) = pid

snkComp :: Edge -> CompID
snkComp (MkEdge (_, _) (Just cid, _)) = cid

srcComp :: Edge -> CompID
srcComp (MkEdge (Just cid, _) (_, _)) = cid

