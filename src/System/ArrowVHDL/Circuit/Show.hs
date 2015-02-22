-- Dieses Modul stellt die Schnittstelle für die Show-Funktionalität dar. Wird \hsSource{Circuit.Show} in einem anderen Modul eingebunden, so
-- können \hsSource{CircuitDescriptor}en mittels \hsSource{show} angezeigt werden. 

module System.ArrowVHDL.Circuit.Show
where

-- %% TODO? Btw, for what ever reason, one can't just reload, because this leads to a 
-- %% missing object-file error. Exit the ghci and restart it, will do the job.

import System.ArrowVHDL.Circuit.Descriptor
-- import System.ArrowVHDL.Circuit.Show.Simple
-- import System.ArrowVHDL.Circuit.Show.VHDL
import System.ArrowVHDL.Circuit.Show.DOT

import qualified System.ArrowVHDL.Circuit.Show.Simple as Simple
import qualified System.ArrowVHDL.Circuit.Show.DOT as DOT
import qualified System.ArrowVHDL.Circuit.Show.VHDL as VHDL

-- An externen Modulen wird hier lediglich die Kerndefinition \hsSource{Circuit.Descriptor} verwendet. Darüber hinaus werden die jeweiligen
-- Anzeigemodule eingebunden. Folgende Formate können als Ausgabeformat gewählt werden:
-- \begin{itemize}
--   \item Mit \begriff{Simple} ist ein Ausgabeformat gemeint, welches sehr kurz und prägnant alle verfügbaren Informationen anzeigt. Dieses
--     Format eignet sich besonders gut, um damit debug-Informationen auszugeben. 
--   \item Die \begriff{DOT}-Sprache ist eine Beschreibungssprache, die verwendet wird, um Graphen abzubilden. Es gibt eine ganze Reihe von
--     Werkzeugen \footnote{Für Linux sei hier \begriff{graphviz} genannt}, die dann aus einer \texttt{.dot}-Datei eine Grafik erzeugen können,
--     die den Graphen abbildet.
--   \item Auch das erzeugen von \begriff{VHDL} ist eine Ausgabeform, die über Haskells \hsSource{Show}-Methoden abgebildet wird. 
-- \end{itemize}
-- 
-- %% To draw a StructGraph it is necessary to make StructGraph an instance of Show and 
-- %% therefore the Edge datatypes also needs to be an instance of Show. 
-- 
-- Um Haskells \hsSource{Show}-Klasse verwenden zu können, muss der Typ \hsSource{CircuitDescriptor} Mitglied der Klasse \hsSource{Show} sein.
-- Da die eigentliche Definition der \hsSource{Show}-Methode in einem Untermodul stattfindet, ist die Instanzdefinition einfach. Es wird
-- lediglich der \hsSource{show}-Methode eine der vorhandenen Methoden zugeordnet. 
-- 

instance Show (Edge) where
    show = showEdge

instance Show (CircuitDescriptor) where
    show = showCircuit
