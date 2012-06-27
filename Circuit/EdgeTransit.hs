-- In diesem Modul wird ein Algebraischer Datentyp aufgebaut. Dieses Datum wird im Codegenerator zur Generierung des Zielcodes benötigt. Der % TODO : Referenz
-- Algebraische Datentyp besteht aus einem Tupel, dass aus einer Liste von Ankerpunkten sowie den dazugehörigen Namen, besteht. 


module Circuit.EdgeTransit
where



-- Im späteren wird die Funktionen aus dem \ref{mod:Circuit.Descriptor} Modul benötigt, sowie die Funktion \hsSource{isJust} aus \hsSource{Data.Maybe}.

import Data.Maybe (isJust)
import Circuit.Descriptor


-- \subsection{Datenstruktur}
-- Die Zwischenstruktur \hsSource{NamedEdge} wird über ein Typalias definiert. Hierbei wird einem Namen eine Liste von Ankerpunkten zugeordnet.

type NamedEdge = ([Anchor], String)



-- \subsection{Funktionen}
-- Zu der Datenstruktur gehören folgende Funktionen:
-- \begin{itemize}
--   \item \hsSource{generateNamedEdges} \\
--         Eine Funktion die aus einem Graphen eine Liste benannter Kanten erstellt
--   \item \hsSource{getAllEdgeNames} \\
--         Eine Funktion welche eine Liste benannter Kanten nutzt, um eine Liste von Namen zu erstellen
--   \item \hsSource{getEdgeName} 
--         Eine Funktion die einen Namen aus der Liste benannter Kanten und einem Ankerpunkt erzeugt
-- \end{itemize}




-- Die Funktion \hsSource{generateNamedEdges} filtert aus dem übergebenen \hsSource{CircuitDescriptor} die Kanten heraus. Von allen Kanten
-- werden die Kanten heraus gefiltert, die nach \begriff{außen} verbinden. Diese Kanten haben als Quell- oder Zielkomponente \hsSource{Nothing}
-- gesetzt. Die relevanten Kanten werden durchnumeriert und in der Form einer \hsSource{NamedEdge} zurückgeliefert. 

pre :: String
pre = nameSig

generateNamedEdges :: CircuitDescriptor -> [NamedEdge]
generateNamedEdges g
    = map (\(i, e) -> (sourceInfo e : sinkInfo e : [], pre ++ show i))
        $ zip [0..] relevantEdges
        where relevantEdges = filter (\  (MkEdge (ci,_) (co,_))
                                      -> isJust ci && isJust co)
                            $ edges g



-- \hsSource{getAllEdgeNames} filtert lediglich das zweite Datum aus dem Tupel heraus. 

getAllEdgeNames :: [NamedEdge] -> [String]
getAllEdgeNames = map snd



-- Zum ermitteln des Namens einer Kante, die an einem bestimmten Ankerpunkt beginnt oder endet, wird ein Funktion \hsSource{getNamedEdge}
-- definiert. Hierzu wird überprüft, ob der übergebene Ankerpunkt in einer der benannten Kanten vorkommt. Ist dies der Fall, wird das erste
-- Element der Ergebnisliste zurückgeliefert. %%% TODO : ist der Anker nicht vorhanden, erzeugt das eine ``empty List'' exception

getNamedEdge :: [NamedEdge] -> Anchor -> NamedEdge
getNamedEdge nedgs ap
      = head
      $ filter (\(aps, _) -> ap `elem` aps)
      $ nedgs



-- Um nur den Namen einer Kante zu ermitteln, die einen bestimmten Ankerpunkt verbindet, wir die Funktion \hsSource{getEdgeName} definiert.
-- Diese filtert den zweiten Wert des Ergebnisstupels von \hsSource{getNamedEdge} heraus und liefert ihn als Rückgabe.

getEdgeName :: [NamedEdge] -> Anchor -> String
getEdgeName nedgs ap
      = snd $ getNamedEdge nedgs ap

