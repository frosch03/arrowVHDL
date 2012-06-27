-- In diesem Modul werden eine Reihe von \begriff{worker}-Funktionen definiert die alle einen übergebenen Wert verändern und die geänderte
-- Variante zurückliefern. 

module Circuit.Workers
where

-- Zur Funktionsdefinition werden Funktionen aus folgenden Modulen benötigt.

import Data.List (nub, (\\))

import GHC.Exts (sortWith)

import Circuit.Descriptor
import Circuit.Sensors
import Circuit.Tests



-- \subsection{CircuitDescriptor Funktionen}
-- Dieser Abschnitt befasst sich mit Funktionen, die auf \hsSource{CircuitDescriptor}en arbeiten. 

-- Mit der Funktion \hsSource{alterCompIDs} lassen sich alle Komponenten IDs innerhalb eines \hsSource{CircuitDescriptor}s verändern. Der erste
-- Parameter legt dabei die kleinst mögliche ID fest. 

alterCompIDs :: Int -> CircuitDescriptor -> CircuitDescriptor
alterCompIDs i sg
    = sg { nodeDesc = nd { nodeId = nodeId nd + i }
         , nodes  = map (alterCompIDs i) $ nodes sg
         , edges  = map (\ (MkEdge (ci,pi) (co,po))
                        -> (MkEdge (maybe ci (Just.(+i)) $ ci ,pi)
                                   (maybe co (Just.(+i)) $ co ,po))
                        ) $ edges sg
         }
     where nd = nodeDesc sg


-- Die Funktion \hsSource{dropCircuit} ermöglicht es, einzelne Schaltkreis-Beschreibungen aus dem \hsSource{CircuitDescriptor} zu entfernen.
-- Hierzu wird eine match-Funktion als erster Parameter erwartet.

dropCircuit :: (CircuitDescriptor -> Bool) -> CircuitDescriptor -> CircuitDescriptor
dropCircuit f sg
    = sg { nodes = newNodes
         , edges = newEdges
         }
    where specific = filter f (nodes sg)
          newEdges = foldl (flip dropEdgesBordering) (edges sg) (map (nodeId.nodeDesc) specific)
          newNodes = map (dropCircuit f) $ nodes sg \\ specific


-- flatten} ist eine Funktion, welche die interne Struktur des \hsSource{CircuitDescriptor}s \begriff{glättet}. Jeder
-- CircuitDescriptor} der nicht Atomar ist, enthält weitere Unterstrukturen. Diese beschreiben, woraus dieser
-- CircuitDescriptor} aufgebaut wird. Enthält der \hsSource{CircuitDescriptor} unnötige Verschachtelungen, werden diese mittels
-- flatten} entfernt. 

-- Als Sonderfall gelten die Schaltungen, die Schleifen darstellen. Hier gibt es keine überflüssigen Verschachtelungen, mindestens aber muss
-- der Algorithmus zum erkennen solcher ein anderer sein, so dass \hsSource{flatten} auf diese Teilbereiche zunächst nicht angewandt werden
-- sollte.  
flatten :: CircuitDescriptor -> CircuitDescriptor
flatten g
    | isLoop g
    = g

    | otherwise
    = g { nodes = nub $ atomCIDs
        , edges =       esBetweenAtoms
        }
    where atomCIDs       = filter isAtomic $ allCircuits g
          esFromAtoms    = concat $ map (fromCompEdges g . nodeId . nodeDesc) atomCIDs
          esFromOuter    = filter isFromOuter $ edges g
          esBetweenAtoms = zipWith MkEdge (map sourceInfo $ esFromOuter ++ esFromAtoms) (map nextAtomOrOut $ esFromOuter ++ esFromAtoms)
          nextAtomOrOut  = (\e -> let (c, p) = nextAtomic g e 
                                  in  if c == mainID then (Nothing, p) else (Just c, p))
          mainID         = nodeId.nodeDesc $ g


-- Die Funktionen \hsSource{dropGenerated} sowie \hsSource{dropID} stellen Spezialfälle der \hsSource{dropCircuit} Funktion dar.
-- dropGenerated} löscht sämtliche \hsSource{CircuitDescriptor}en, die automatisch generiert wurden. Ebenso löscht \hsSource{dropID}
-- CircuitDescriptor}en, die den \hsSource{isID}-Test bestehen. \hsSource{isID} sowie \hsSource{isGenerated} sind im Modul
-- \ref{mod:Circuit.Tests} beschrieben.

dropGenerated :: CircuitDescriptor -> CircuitDescriptor
dropGenerated = dropCircuit isGenerated

dropID :: CircuitDescriptor -> CircuitDescriptor
dropID = dropCircuit isID



-- Diese Funktionen arbeiten auf \hsSource{CircuitDescriptor}en, und erzeugen Kanten, oder es handelt sich um Funktionen, die aus bestehenden
-- Kanten neue generieren. 


-- Mit der Funktion \hsSource{connectCID} lassen sich zwei \hsSource{CircuitDescriptor}en miteinander verbinden. Dabei werden zwei
-- \hsSource{CircuitDescriptor}en übergeben, sowie die Quell-Komponenten ID und die Ziel-Komponenten ID zusammen mit einer Ziel-Pin ID. Erzeugt
-- wird eine Kante, welche die Verbindung zwischen beiden \hsSource{CircuitDescriptor}en darstellt. Von der Quelle wird keine \hsSource{PinID}
-- benötigt, da hier auf den nächst freien Pin zurückgegriffen wird. Auf der Ziel-Seite ist es dann aber notwendig, einen Ziel-Pin zu
-- definieren.

connectCID :: CircuitDescriptor -> CircuitDescriptor -> CompID -> (CompID, PinID) -> Edge
connectCID old_g g cidF (cidT,pidT)
    = MkEdge (Just cidF, nextFpin) (Just cidT, pidT)
    where nextFpin  = head $ drop cntEsFrom $ sources.nodeDesc $ getComp old_g cidF
          cntEsFrom = length $ filter (\x -> (not.isFromOuter $ x) && (srcComp x == cidF)) $ edges g


-- Zum entfernen von Kanten die an eine Komponente angrenzen, ist die Funktion \hsSource{dropEdgesBordering} da. Übergeben wird die ID der
-- Komponente, die heraus gelöst werden soll, sowie die Liste mit den betroffenen Kanten. Es wird dann eine neue List mit Kanten erstellt, die
-- nicht mehr zu der Komponente mit der besagten ID führen. Alle Kanten die nicht mehr an einer Komponente andocken, werden zusammengefügt.
-- Diese Funktion kann nur dann funktionieren, wenn die zu lösende Komponente genausoviele eingehende Pins, wie ausgehende Pins besitzt. 
-- %%% TODO : Components with different InPinCount and OutPinCount have a PROBLEM 

dropEdgesBordering :: CompID -> [Edge] -> [Edge]
dropEdgesBordering cid es
    = (es ++ mergeEdges (toIt, fromIt)) \\ (toIt ++ fromIt)
    where toIt   = filter ((== (Just cid)).fst.sinkInfo)   $ es
          fromIt = filter ((== (Just cid)).fst.sourceInfo) $ es


-- \hsSource{mergeEdges} ist eine Funktion, die zwei Listen mit Kanten entgegennimmt und diese beiden zusammenfasst. Kanten die auf einen Pin
-- enden und Kanten die vom gleichen Pin starten, werden zu einer Kante zusammengefasst. 

mergeEdges :: ([Edge], [Edge]) -> [Edge]
mergeEdges (xs, ys)
    = zipWith (\x y -> MkEdge (sourceInfo x) (sinkInfo y)) xs' ys'
    where x_snkPins = map snkPin xs
          y_srcPins = map srcPin ys
          xs'       = sortWith snkPin $ filter (\edg -> (snkPin edg) `elem` y_srcPins) xs
          ys'       = sortWith srcPin $ filter (\edg -> (srcPin edg) `elem` x_snkPins) ys


-- Mit der \hsSource{fillEdgeInfoCompID} Funktion, lassen sich die Quell- und Ziel-Komponenten IDs in Kanten setzen, in denen bis dahin
-- \hsSource{Nothing} als Wert gespeichert ist. Dies ist dann notwendig, wenn eine neue Komponente in eine bestehende Struktur eingefügt wird.
-- Dies wird dann benötigt, wenn eine Komponente in eine Struktur eingefügt werden soll. Eine noch nicht integrierte Komponente bekommt ihre %% TODO : Werte ist sicher nicht das richtige wort hier
-- Werte von einer unbekannte Komponente (\hsSource{Nothing}) und liefert die Ergebnisse auch an \hsSource{Nothing}. Wird sie nun eine
-- Unterkomponente, so kann das \hsSource{Nothing} durch eine tatsächliche Komponenten ID ersetzt werden. 

fillEdgeInfoCompID :: CompID -> Edge -> Edge
fillEdgeInfoCompID cid (MkEdge (Nothing, srcPid) (snkInfo)) = (MkEdge (Just cid, srcPid) (snkInfo))
fillEdgeInfoCompID cid (MkEdge (srcInfo) (Nothing, snkPid)) = (MkEdge (srcInfo) (Just cid, snkPid))
fillEdgeInfoCompID _   e = e


-- Ein ähnliches Problem wie \hsSource{fillEdgeInfoCompID} wird auch von den Funktionen \hsSource{fillSrcInfoCompID} und
-- \hsSource{fillSnkInfoCompID} gelöst. Diese unterscheiden sich lediglich darin, dass diese Funktionen jeweils nur die Quell-Pins oder nur die
-- Ziel-Pins betreffen. 

fillSrcInfoCompID :: CompID -> Edge -> Edge
fillSrcInfoCompID cid (MkEdge (Nothing, srcPid) (snkCid, snkPid))
    = (MkEdge (Just cid, srcPid) (snkCid, snkPid))

fillSnkInfoCompID :: CompID -> Edge -> Edge
fillSnkInfoCompID cid (MkEdge (srcCid, srcPid) (Nothing, snkPid))
    = (MkEdge (srcCid, srcPid) (Just cid, snkPid))
