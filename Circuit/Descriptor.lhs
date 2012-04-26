\section{Eine Komponente}
\label{mod:Circuit.Descriptor}

Im folgenden wird der das Modul \hsSource{Circuit.Descriptor} beschrieben
\begin{code}
  module Circuit.Descriptor
  where
\end{code}

\subsection{Typen mit neuen Namen}
Zunächst werden grundlegende Typaliasse vergeben, die im gesamten Quelltext Anwendung finden.

\par
Jede Hardware Komponente besitzt Pins verschiedener Art, darunter ein- und ausgehende. Diese sind durchnummeriert und werden über ihre
Nummer, die als Integer abgebildet wird, identifiziert. Für die Komponenten ID gilt dies ebenfalls.

\par 
Daneben wird der Typ für einen \begriff{Clock-Cycle} definiert. In diesem Fall wird ein Integer dafür verwendet. Auch die Fläche wird mit
einem Integer beschrieben. Für die Fläche besagt der Wert, wieviele \begriff{Zellen} der Baustein belegt. %%% TODO : Was ist eine Zelle?
\begin{code}
  type PinID = Int
  type Pins  = [PinID]

  type ID     = Int
  type CompID = ID

  type Tick   = Int
  type Area   = Int
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

\subsection{Schaltungsbeschreibung}
Jede Komponente die durch einen Arrow repräsentiert wird, hat zusätzliche Attribute, die nicht in dem Arrow selber stehen. Diese Attribute
werden auch nicht für alle Arrow-Klassen Instanzen benötigt. Daher sind diese lose an den Arrow gekoppelt. \footnote{Arrow und Attribute
werden in einem Tupel zusammengefasst}

\par
Hier wird unterschieden zwischen Kombinatorischen Schaltungen \hsSource{MkCombinatorial}, Registern \hsSource{MkRegister} und nicht
vorhandenen Schaltungen \hsSource{NoDescriptor}. %%% TODO : Wie fließt hier MkComposite rein?

\par
Ein Register unterscheidet sich von einer kombinatorischen Schaltung, teilweise gibt es Gemeinsamkeiten. Diese Gemeinsamkeiten werden über
den Datentyp \hsSource{NodeDescriptor} verkörpert.


\begin{code}
  data NodeDescriptor
    = MkNode
      { label   :: String 
      , nodeId  :: ID
      , sinks   :: Pins
      , sources :: Pins
      }
    deriving (Eq)
\end{code} 


\par
Der \hsSource{NodeDescriptor} taucht in der Definition eines Schaltkreises, aber auch in der Definition eines Registers, auf.

\begin{code}
  data CircuitDescriptor
    = MkCombinatorial
      { nodeDesc :: NodeDescriptor
      , nodes    :: [CircuitDescriptor]
      , edges    :: [Edge]
      , cycles   :: Tick
      , space    :: Area
      }

    | MkRegister
      { nodeDesc :: NodeDescriptor
      , bit      :: Int
      }

    | MkLoop
      { nodeDesc :: NodeDescriptor
      , nodes    :: [CircuitDescriptor]
      , edges    :: [Edge]
      , cycles   :: Tick
      , space    :: Area
      }

--  | MkComposite

    | NoDescriptor
    deriving (Eq)

  type Netlist = CircuitDescriptor
\end{code}

In Haskell lassen sich die Komponenten Attribute über einen Summentyp abgebildet. Dieser Datentyp ist ein fundamentaler Datentyp, da er
gleichberechtigt neben der eigentlichen Funktionalität steht. Er besitzt einen Bezeichner \hsSource{label}, sowie eine eindeutige ID
\hsSource{nodeId}. Zusätzlicher sind die Ein- sowie die Ausgehenden Pins aufgeführt und auch die verbindenden Kanten (\hsSource{edges}).

\par 
Jede Komponente kann durch untergeordnete Komponenten beschrieben werden. Dies wird im Datentyp über die \hsSource{nodes} Liste abgebildet.
Ist die Komponenten atomar, so enthält dieses Datum die leere Liste. Der Konstruktor \hsSource{NoSG} ist analog zu $\varnothing$. %%% TODO : Mathematisch erklären warum {} benötigt wird

\par
Um zu verhindern, dass ungültige Schaltungsbeschreibungen erzeugt werden, können \begriff{smart constructor}s eingesetzt werden. Hierbei
handelt es sich um Funktionen, die analog zu den Konstruktoren arbeiten. Diese unterscheiden sich darin, dass die Konstruktoren jedes Datum
erzeugen. Allerdings ist es häufig nicht gewünscht, jedes Datum erzeugen zu können oder es ist gewünscht, dass der Benutzer beim Versuch ein
falsches Datum zu erzeugen, mit einer Fehlermeldung konfrontiert wird. 

\par
Es folgt der Quellcode des \begriff{smart constructor}s für ein Register. Hierbei ist nur darauf zu achten, dass dieses Register keine ID
bekommt, die schon einmal vergeben wurde.

%%% TODO : error checking muss hier noch rein

\begin{code}
  mkRegister :: NodeDescriptor -> CircuitDescriptor
  mkRegister nd 
    = MkRegister  
      { nodeDesc = nd { label = "REG" ++ (show $ nodeId nd) } 
      , bit      = length $ sinks nd
      }
\end{code} 


\subsection{Kanten}
Zuletzt fehlt jetzt lediglich die Definition einer Kante. Eine Kante kennt einen Quell-Pin sowie einen Ziel-Pin.
\begin{code}
  data Edge
    = MkEdge { sourceInfo :: SourceAnchor
             , sinkInfo   :: SinkAnchor
             }
    deriving (Eq)
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
