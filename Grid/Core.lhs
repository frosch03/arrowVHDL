\section{Eine Komponente}
\label{src:component}

Im folgenden wird der das Modul \hsSource{Grid.Core} beschrieben
\begin{code}
  module Grid.Core
  where
\end{code}

\subsection{Typen mit neuen Namen}
Zunächst werden grundlegende Typaliasse vergeben, die im gesamten Quelltext Anwendung finden.

\par
Jede Hardware Komponente besitzt Pins verschiedener Art, darunter ein- und ausgehende. Diese sind durchnummeriert und werden über ihre
Nummer, die als Integer abgebildet wird, identifiziert. Für die Komponenten ID gilt dies ebenfalls.
\begin{code}
  type PinID = Int
  type Pins  = [PinID]

  type CompID = Int
\end{code}

\par
Eine Kante ist eine Verbindung zwischen zwei \hsSource{Pins}, dabei müssen die beiden \hsSource{Pins} nicht unbedingt zu unterschiedlichen
Komponenten gehören \footnote{Beispielsweise bei zyklischen Schaltungen}. Das Tupel aus \hsSource{PinID} und \hsSource{CompID} identifiziert
einen Pin. Da auch Kanten abgebildet werden sollen, die nach außerhalb der aktuellen Komponente gehen sollen, ist es notwendig, die
\hsSource{CompID} als \hsSource{Maybe} Typ zu beschreiben. Das Tupel wird im folgenden mit \hsSource{Anchor} bezeichnet.
\begin{code}
  type Anchor       = (Maybe CompID, PinID)
  type SinkAnchor   = Anchor
  type SourceAnchor = Anchor
\end{code}

Außerdem wurden noch zwei weitere Aliasse vergeben, die Eingehende (\hsSource{SinkAnchor}) und Ausgehende (\hsSource{SourceAnchor}) Pins
voneinander unterscheiden.

\subsection{Benannte Typen}
Um die Graph-Struktur später in Sourcecode überführen zu können, benötigt man Namen für \hsSource{Anchor} und \hsSource{Edges} sowie
Lookup-Tabellen, in denen dann die Namen abgelegt werden. Die Typaliasse hierfür werden hier direkt definiert.
\begin{code}
  type NamedPins = [(String, Anchor)]
  type NamedSigs = [(String, Edge)]
  type NamedSnks = NamedPins
  type NamedSrcs = NamedPins
  type NamedIOs  = (NamedSnks, NamedSrcs)

  nameSig = "i"
  nameExI = "inc"
  nameExO = "out"
  nameInI = "e"
  nameInO = "a"
\end{code}


\subsection{Schaltungsbeschreibung}
Jede Komponente die durch einen Arrow repräsentiert wird, hat zusätzliche Attribute, die nicht in dem Arrow selber stehen. Diese Attribute
werden auch nicht für alle Arrow-Klassen Instanzen benötigt. Daher sind diese lose an den Arrow gekoppelt. \footnote{Arrow und Attribute
werden in einem Tupel zusammengefasst}

%%% TODO : is Circuit the right name? Component is better or: Chip / Device / Module / what ever
%%% TODO : MkSG und NoSG sind die falschen Konstruktoren



\subsection{Pins}
Zunächst werden grundlegende Typaliasse vergeben, die im gesamten Quelltext Anwendung finden. 

\par
Jede Hardware Komponente besitzt Pins verschiedener Art, darunter Eingabe- und Ausgabepins.

\subsection{Circuit}
Jede Komponente die durch einen Arrow repräsentiert wird, hat zusätzliche Attribute, die nicht in dem Arrow selber stehen. Diese Attribute
werden auch nicht für alle Arrow-Klassen Instanzen benötigt. Daher sind diese lose an den Arrow gekoppelt. \footnote{Arrow und Attribute
werden in einem Tupel zusammengefasst}

%%% TODO : is Circuit the right name? Component is better or: Chip / Device / Module / what ever
%%% TODO : MkSG und NoSG sind die falschen Konstruktoren

\begin{code}
  data CircuitDescriptor
    = MkDescriptor 
           { label    :: String 
           , compID   :: CompID
           , nodes    :: [CircuitDescriptor]
           , edges    :: [Edge]
           , sinks    :: Pins
           , sources  :: Pins
           }
    | NoDescriptor
    deriving (Eq)

  type Netlist = CircuitDescriptor
\end{code}

In Haskell lassen sich die Komponenten Attribute über einen Summentyp abgebildet. Dieser Datentyp ist ein fundamentaler Datentyp, da er
gleichberechtigt neben der eigentlichen Funktionalität steht. Er besitzt einen Bezeichner \hsSource{label}, sowie eine eindeutige ID
\hsSource{compID}. Zusätzlicher sind die Ein- sowie die Ausgehenden Pins aufgeführt und auch die verbindenden Kanten (\hsSource{edges}).

\par 
Jede Komponente kann durch untergeordnete Komponenten beschrieben werden. Dies wird im Datentyp über die \hsSource{nodes} Liste abgebildet.
Ist die Komponenten atomar, so enthält dieses Datum die leere Liste. Der Konstruktor \hsSource{NoSG} ist analog zu \varnothing. %%% TODO : Mathematisch erklären warum {} benötigt wird


\subsection{Kanten}
Zuletzt fehlt jetzt lediglich die Definition einer Kante. Eine Kante kennt einen Quell-Pin sowie einen Ziel-Pin.
\begin{code}
  data Edge
    = MkEdge { sourceInfo :: SourceAnchor
             , sinkInfo   :: SinkAnchor
             }
    deriving (Eq)
\end{code}

