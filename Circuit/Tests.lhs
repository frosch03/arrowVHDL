\long\def\ignore#1{}

\section{Test Funktionen}
\label{mod:Circuit.Tests}

Dieses Modul beschreibt eine Reihe von Funktionen zum prüfen von Eigenschaften. Diese Funktionen haben eine Gemeinsamkeit, nämlich dass sie
letztlich immer einen \hsSource{Bool}-Wert erzeugen. 


\begin{code}
  module Circuit.Tests 
  where
\end{code} 

\par 
Lediglich das Modul \hsSource{Circuit.Descriptor} wird benötigt


\begin{code}
  import Circuit.Descriptor
\end{code} 

\subsection{Tests mit Kantenbezug}

Die Funktion \hsSource{isFromOrToComp} ermittelt ob eine übergebene Kante eine Verbindung zu einer Komponente, die über ihre Komponenten ID
identifiziert wird, darstellt.

\begin{code}
  isFromOrToComp :: CompID -> Edge -> Bool
  isFromOrToComp cid (MkEdge (Nothing, pi) (Just co, po)) = cid == co
  isFromOrToComp cid (MkEdge (Just ci, pi) (Nothing, po)) = cid == ci
  isFromOrToComp cid (MkEdge (Just ci, pi) (Just co, po)) = cid == co 
\end{code} 


\par
Jeder Schaltkreis besitzt eine innere Verschaltung und daneben eine Verbindung zur Außenwelt. Eine Kante kann von ``Außen'' her kommen, oder
nach ``Außen'' gehen. Genau diese Kanten sind die Pins eines tatsächlichen Chips. 


\par Mit \hsSource{isToOuter} wird getestet, ob es sich um eine abgehende Kante, also einen ``outgoing''-Pin handelt.

\begin{code}
  isToOuter :: Edge -> Bool
  isToOuter (MkEdge (_, _) (Nothing, _)) = True
  isToOuter _                            = False
\end{code} 


\par Das Pendant zu abgehenden Kanten sind eingehende Kanten. Diese werden auch als Inputs bezeichnet. \hsSource{isFromOuter} testet, ob
eine Kante eingehend ist.

\begin{code}
  isFromOuter :: Edge -> Bool
  isFromOuter (MkEdge (Nothing, _) (_, _)) = True
  isFromOuter _                            = False
\end{code} 



\subsection{Test mit Schaltkreisbezug}

Mit der Funktion \hsSource{hasLabel} wird überprüft, ob ein Schaltkreis den übergebenen Namen trägt.

\begin{code}
  hasLabel :: String -> CircuitDescriptor -> Bool
  hasLabel s
      = ((== s).label.nodeDesc)
\end{code} 


\par
\hsSource{isAtomic} überprüft, ob ein Baustein atomar ist, also ob er aus weiteren Bausteinen zusammengesetzt ist, oder nicht.

\begin{code}
  isAtomic :: CircuitDescriptor -> Bool
  isAtomic g
      = if (length (nodes g) == 0) then True else False
\end{code} 


\par
Die Funktion \hsSource{isSuperNodeOf} testet, ob eine Komponente eine bestimmte andere Komponente, identifiziert durch ihre Komponenten ID,
enthält.

\begin{code}
  isSuperNodeOf :: CircuitDescriptor -> CompID -> Bool
  isSuperNodeOf g cid 
      = if length (filter (== cid) subNodes) > 0
            then True
            else False
      where subNodes = map (nodeId.nodeDesc) $ nodes g
\end{code} 


\par 
Mit der Funktion \hsSource{isGenerated} lässt sich herausfinden, ob einen Komponente eine vom Entwickler entwickelte Komponente ist, oder ob
diese Komponente automatisch vom System erzeugt wurde.

\begin{code}
  isGenerated :: CircuitDescriptor -> Bool
  isGenerated s = ((== '|').head.label.nodeDesc) s && ((== '|').head.reverse.label.nodeDesc) s
\end{code} 



%%% TODO : isID ist nur eine debugging funktion und muss rausgenommen werden
\ignore{
\begin{code}
  isID :: CircuitDescriptor -> Bool
  isID = hasLabel "-ID-"
\end{code} 
}
