\section{Werkzeuge}
\label{mod:Grid:Tools}

Dieses Modul stellt Funktionen zur Verfügung die beim erstellen von Beispielen nützlich sind. 

\begin{code}
  module Grid.Tools
  where
\end{code}


\par
Eingebunden werden für die Demo-Funktionen aus den Systembibliotheken die folgenden Module:

\begin{code}
  import System.IO (writeFile)
  import System.Cmd (system)
\end{code}

Daneben wird noch der \begriff{except}-Operator (\hsSource{(\\)}) aus dem Listen-Modul benötigt:

\begin{code}
  import Data.List ((\\))
\end{code}


\par
Schließlich werden eine ganze Reihe von Modulen aus der \hsSource{Grid}-Reihe verwendet:

\begin{code}
  import Grid.Core
  import Grid.Show
  import Grid.Graph
  import Grid.Auxillary
  import Grid.Show.DOT
  import Grid.Workers (mergeEdges)
  import Grid.Tests
\end{code}


\subsection{Demo Funktionen}
Mit \hsSource{write} und \hsSource{genPicture} werden zwei Funktionen definiert, die auf das Dateisystem zugreifen, und letztliche dazu
verwendet werden, eine Grafik einer Schaltung zu erstellen. Dazu wird mittels \hsSource{show} die \begriff{.dot}-Darstellung des übergebenen
Schaltkreises erzeugt und im \hsSource{/tmp}-Folder abgespeichert. 

\par
\hsSource{genPicture} greift dann auf die soeben erzeugte Datei zu, und erzeugt aus dem \begriff{.dot}-File ein Bild mit der Grafik. Hierbei
wird vorausgesetzt, das auf dem System die \begriff{graphviz}-Umgebung vorinstalliert ist.

\begin{code}
  write x    = writeFile "/tmp/test.dot" (Grid.Show.DOT.showCircuit x)
  genPicture = system "dot /tmp/test.dot -Tjpg -o /tmp/test.jpg"
\end{code}



\par
Mit der Funktion \hsSource{toCircuit} lässt sich aus einer minimal Definition ein gültiger Schaltkreis erzeugen. Die minimale Definition
muss dabei wenigstens einen Namen, sowie die Anzahl der eingehenden und Ausgehenden Pins enthalten.

\begin{code}
  toCircuit :: (String, Int, Int) -> CircuitDescriptor
  toCircuit (name, inPins, outPins)
      = emptyCircuit { label   = name
                   , sinks   = [0..(inPins -1)]
                   , sources = [0..(outPins -1)]
                   }
\end{code}


\par
Die Funktion \hsSource{filterByName} ist ein Beispiel für die Mächtigkeit des neuartigen Ansatzes. \hsSource{filterByName} geht hier über
eine Schaltung hinweg, und nimmt alle Vorkommenden Bausteinen mit einem gewissen Namen heraus. So lassen sich zum Testen sehr leicht von
den enthaltenen Schaltungen, gewisse selbige durch eine andere Version ersetzen.

\begin{code}
  filterByName :: CircuitDescriptor -> String -> CircuitDescriptor
  filterByName s n 
      = if (label s == n) && (length (nodes s) < 1) 
          then NoDescriptor
          else s { nodes = (map (flip filterByName n) $ nodes s) }
\end{code}


\par
Zur Anwendung wird \hsSource{filterByName} in der nächsten Funktion, nämlich in \hsSource{replace} gebracht. Hier übergibt man die Schaltung
in der man ändern möchte. Außerdem übergibt man ein Tupel bestehenden aus einem \hsSource{from}-Baustein und einem \hsSource{to}-Baustein,
wobei nach dem \hsSource{from}-Baustein gesucht wird, und dieser dann mit dem \hsSource{to}-Baustein ersetzt wird. Als Ergebnis erhält man
eine veränderte Schaltung

\begin{code}
  replace :: CircuitDescriptor -> (CircuitDescriptor, CircuitDescriptor) -> CircuitDescriptor
  replace s ft@(from, to) 
      | not $ isAtomic s
      = s { nodes = map (flip replace $ ft) (nodes s) }
      
      |  label s            == label from
      && length (sinks   s) == length (sinks   from)
      && length (sources s) == length (sources from)
      && length (sinks   s) == length (sinks   to)
      && length (sources s) == length (sources to)
      = to { compID = compID s }
  
      | otherwise = s
\end{code}

%% TODO : Programmiere: mark / cut / trim



\par
Die Funktion \hsSource{bypass} ermöglicht eine ähnlich Funktionalität, wie auch schon \hsSource{filterByName} oder \hsSource{replace}.
Allerdings nimmt \hsSource{bypass} nur die gefundenen Bausteine aus der Schaltung heraus.

%%% TODO : Erklären was rebuildIf macht, und warum ;) 
%%% TODO : grep wires usw. ist teil von rebuildIf

\begin{code}
  bypass :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  bypass s item 
    = s { nodes = ns
        , edges = es
        }
    where (es, ns) = foldl (rebuildIf (\x -> label x == label item)) (edges s, []) $ nodes s


  rebuildIf :: (CircuitDescriptor -> Bool) ->  ([Edge], [CircuitDescriptor]) -> CircuitDescriptor -> ([Edge], [CircuitDescriptor])
  rebuildIf isIt (super_es, new_ns) NoDescriptor = (super_es, new_ns)
  rebuildIf isIt (super_es, new_ns) n
      |  isIt n       && length (sinks n) == length (sources n)
      = (new_es , new_ns)
  
      | otherwise 
      = (super_es, new_ns ++ [n'])
  
      where new_es  = (super_es \\ (lws ++ rws)) ++ nws
            lws     = leftWires super_es (compID n)
            rws     = rightWires super_es (compID n)
            nws     = zipWith MkEdge (map sourceInfo lws) (map sinkInfo rws)
            (es,ns) = foldl (rebuildIf isIt) (edges n, []) $ nodes n
            n'      = n { nodes = ns
                        , edges = es
                        }


  grepWires :: (Edge -> Anchor) -> [Edge] -> CompID -> [Edge]
  grepWires f es cid = filter (\e -> fst (f e) == Just cid) es

  leftWires :: [Edge] -> CompID -> [Edge]
  leftWires = grepWires sinkInfo

  rightWires :: [Edge] -> CompID -> [Edge]
  rightWires = grepWires sourceInfo

  solderWires :: ([Edge], [Edge]) -> [Edge]
  solderWires = mergeEdges
\end{code}
