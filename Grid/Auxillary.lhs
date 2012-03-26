\section{Hilfunktionalitäten}
\label{mod:Grid:Auxillary}

Das Hilfsmodul stellt ein Sammelbecken für alle Funktionen dar, die nicht in eines der anderen Module gepasst haben.

\begin{code}
  module Grid.Auxillary 
  where
\end{code}


\par
Da dieses Modul keine spezielle Aufgabe besitzt sind auch die Aufgaben der einzelnen Funktionen sehr unterschiedlich. Aus diesen Gründen ist
die Liste der eingebundenen Module recht lang. 

\begin{code}
  import Data.List (union, groupBy, isInfixOf)
  import Data.Maybe
  import Data.Either
  import Control.Monad (msum)
  
  import GHC.Exts (sortWith)
  
  import Grid.Core
  import Grid.Show
  import Grid.Tests
  import Grid.Splice
  import Grid.Sensors
  import Grid.Workers
  
  import Grid.Graph (emptyCircuit)
\end{code}


\subsection{Verdrahtungsvarianten}
Die Funktion \hsSource{splice} aus dem Modul \ref{mod:Grid:Splice} verwendet für das ``verdrahten'' eine der folgenden
\hsSource{rewire}-Funktionen. Daneben wird noch eine Zeichenkette zugeordnet, um später debug-Ausgaben erzeugen zu können.

\par 
Die \hsSource{connect} Funktion verbindet zwei Schaltkreise zu einem neuen. Hierbei wird sequentiell verbunden, als Zeichenkette wird der
selbe Operator angegeben, wie er auch aus der \hsSource{Arrow}-Schreibweise schon bekannt ist.

\begin{code}
  connect :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  connect = splice (seqRewire, ">>>")
\end{code}


\par
Neben dem sequentiellen verbinden lassen sich Schaltkreise auch parallel verbinden. Dies ist mit der Funktion \hsSource{combine} möglich.
Als Zeichenkette wird auch hier das aus der \hsSource{Arrow}-Schreibweise bekannte Operator-Symbol verwendet. 
%%% TODO : combine = frame???

\begin{code}
  combine :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  combine = splice (parRewire, "&&&")
\end{code}


\par
Eine Variante der \hsSource{combine} Funktion ist die Funktion \hsSource{dupCombine}. Hier werden die Eingänge zunächst dupliziert und dann
parallel weiter verbunden. 

\begin{code}
  dupCombine :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  dupCombine = splice (dupParRewire, ">2>")
\end{code}


\subsection{Smarte Konstruktoren}
In Haskell lassen sich Typen sehr fein granular definieren. Für die Liste der Pins einer Schaltung passt eine Liste von Integer-Werten sehr
gut aber nicht perfekt. So gibt es auch Integer-Listen mit keinem Inhalt. Wollte man das auf der Typebene verhindern, so würde dies einen
Overhead erfordern, der nicht im Verhältnis zu dem Nutzen stehen würde. Eine einfache aber nützliche Alternative sind \begriff{smart
constructor}. Hierbei handelt es sich um einen Funktion, die als Parameter alle Werte bekommt, die benötigt werden, um ein Datum des
gewünschten Types zu erzeugen. Die Funktion erzeugt dann ein solches Datum und kann sich daneben auch noch um Fehler-Behandlungen kümmern.
%%% TODO : Boxed Types reference

\par
Bei \hsSource{mkPins} handelt es sich um so einen \begriff{smart Constructor}. Dieser erhält die Anzahl der benötigten Pins als Parameter
und erzeugt dann eine entsprechende Liste. 

\begin{code}
  mkPins :: Int -> Pins
  mkPins 0 = error $ show "It is not possible to generate a component with 0 pins"
  mkPins n = [0..n-1]
\end{code}


\par %%% Unter Ferner liefen XXX
Mit \hsSource{nextID} hat man eine Funktion, die eine Liste vom Komponenten Nummer erhält und daraus dann eine nächste gültige Komponenten
Nummer erzeugt.

\begin{code}
  nextID :: [CompID] -> CompID
  nextID []    = 0
  nextID [cid] = cid + 1
  nextID cids  = nextID [foldl max 0 cids]
\end{code}


\par %%% Unter Ferner liefen XXX 
Die Funktion \hsSource{onlyInnerEdges} filtert aus einer Liste von Kanten genau diese Kanten heraus, die die internen Kanten im Schaltkreis
darstellen. Die Ergebnismenge enthält keine Ein- und Ausgehenden Kanten.

\begin{code}
  onlyInnerEdges :: [Edge] -> [Edge]
  onlyInnerEdges es = es'
      where es' = filter notIO $ es
            notIO :: Edge -> Bool
            notIO (MkEdge (Nothing, _) _) = False
            notIO (MkEdge _ (Nothing, _)) = False
            notIO _                       = True
\end{code}


\subsection{Verdrahtungs-Vorstufen}
Unter den \hsSource{rewire}-Funktionen sind Funktionen zu verstehen, die eine Vorstufe für die eigentliche Verbindung (das
\begriff{splicen}) darstellen. Zwei Schaltkreise werden jeweils in eine Zwischendarstellung überführt. Die Zwischendarstellung besteht aus
einer Liste von neuen Kanten (\hsSource{[Edge]}), zusammen mit den überbleibenden Ein- und Ausgangspins. 

\par
Alle \hsSource{rewire}-Funktionen nutzen eine Funktion, nämlich \hsSource{wire}. Das verbinden von Drähten mit Komponenten ist, unabhängig
davon ob sequentiell oder parallel verbunden werden soll, immer gleich. Eingehende Parameter zu \hsSource{wire} sind die beiden Komponenten
Nummern, sowie die Pin-Listen. Auch diese Funktion erzeugt die schon beschriebene Zwischendarstellung.

\begin{code}
  wire :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
  wire cid_l cid_r pins_l pins_r 
      = (edges, (drop cnt pins_l, drop cnt pins_r))
      where points_l = map ((,) (cid_l)) pins_l
            points_r = map ((,) (cid_r)) pins_r
            edges    = map (uncurry MkEdge) $ zip points_l points_r
            cnt      = length edges
\end{code}
  

\par 
\hsSource{wire_} ist ein Synonym für \hsSource{fst . wire}.

\begin{code}
  wire_ :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> [Edge]
  wire_ cid_l cid_r pins_l pins_r = fst $ wire cid_l cid_r pins_l pins_r
\end{code}


\par
Bei der Funktion \hsSource{seqRewire} werden die Verbindungen sequentiell erstellt; übrige Ein oder Ausgänge werden zu den gesamt Ein und
Ausgängen hinzugefügt. 

\begin{code}
  seqRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
  seqRewire sg_l sg_r
      = ( fromOuterToL ++ fromOuterToR ++ edgs ++ fromRToOuter ++ fromLToOuter
        , (super_srcs, super_snks)
        )
      where (edgs, (srcs_l', snks_r')) =  wire (Just $ compID sg_l) (Just $ compID sg_r) (sources sg_l) (sinks sg_r)
            super_srcs                 =  [0..(length.sinks   $ sg_l) + length snks_r' -1]
            super_snks                 =  [0..(length.sources $ sg_r) + length srcs_l' -1]
            ( fromOuterToL, (super_srcs', _)) =  wire Nothing (Just $ compID sg_l) super_srcs  (sinks sg_l)
            ( fromOuterToR, (_          , _)) =  wire Nothing (Just $ compID sg_r) super_srcs' (drop (length fromOuterToL) $ sinks sg_r)
            ( fromRToOuter, (_, super_snks')) =  wire (Just $ compID sg_r) Nothing (sources sg_r) super_snks
            ( fromLToOuter, (_, _))           =  wire (Just $ compID sg_l) Nothing (drop (length fromRToOuter) $ sources sg_l) super_snks'
\end{code}


\par
Bei der \hsSource{parRewire} Funktion werden beide Bausteine ``übereinander'' angeordnet. Die Eingänge beider Komponenten, sowie deren
Ausgänge werden parallel geschaltet. 

\begin{code}
  parRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
  parRewire sg_u sg_d
      = ( goingIn_edges ++ goingOut_edges
        , (super_srcs, super_snks)
        )
      where super_srcs = [0..(length $ (sinks   sg_u) ++ (sinks   sg_d)) -1]
            super_snks = [0..(length $ (sources sg_u) ++ (sources sg_d)) -1]
            goingIn_edges  =  (wire_ Nothing (Just $ compID sg_u)                            (super_srcs) (sinks sg_u))
                           ++ (wire_ Nothing (Just $ compID sg_d) (drop (length.sinks $ sg_u) super_srcs) (sinks sg_d))
            goingOut_edges =  (wire_ (Just $ compID sg_u) Nothing (sources sg_u)                              (super_snks))
                           ++ (wire_ (Just $ compID sg_d) Nothing (sources sg_d) (drop (length.sources $ sg_u) super_snks))
\end{code}


\par
Die Funktion \hsSource{dupParRewire} funktioniert dabei analog zur Funktion \hsSource{parRewire}. Lediglich die Eingänge werden zunächst
dupliziert und dann auf beide Komponenten geschaltet.

\begin{code}
  dupParRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
  dupParRewire sg_u sg_d
      = ( goingIn_edges ++ goingOut_edges
        , (super_srcs, super_snks)
        )
      where super_srcs = [0..(length.sinks $ sg_u) -1]
            super_snks = [0..(length $ (sources sg_u) ++ (sources sg_d)) -1]
            goingIn_edges  =  (wire_ Nothing (Just $ compID sg_u) super_srcs (sinks sg_u))
                           ++ (wire_ Nothing (Just $ compID sg_d) super_srcs (sinks sg_d))
            goingOut_edges =  (wire_ (Just $ compID sg_u) Nothing (sources sg_u)                              (super_snks))
                           ++ (wire_ (Just $ compID sg_d) Nothing (sources sg_d) (drop (length.sources $ sg_u) super_snks))
\end{code}
