\section{Benannte Pins}
\label{mod:Grid:PinTransit}
Dieses Modul definiert einen Algebraischen Datentyp der im Codegenerator zur Generierung des Zielcodes benötigt. Er enthält die % TODO : Referenz
Komponenten ID, sowie benannte Ein- und Ausgangspins.

\begin{code}
  module Grid.PinTransit
  where
\end{code}


\par
Benötigt werden die Definitionen aus dem \ref{mod:Grid:Core} (\hsSource{Grid.Core}) Module benötigt.

\begin{code}
  import Grid.Core
\end{code}

\subsection{Datenstruktur}
Zunächst werden Typaliasse angelegt, welche die verwendeten Datentypen nach ihrer Aufgabe benennen. 

\begin{code}
  type NamedPin  = (PinID, String)
  type InNames   = [NamedPin]
  type OutNames  = [NamedPin]

  type NamedComp = (CompID, (InNames, OutNames))
\end{code} 


\subsection{Funktionen}
Um die interne Datenstruktur des Algebraischen Datentypes bedienen zu können, werden folgende Funktionen definiert

\begin{itemize}
  \item \hsSource{generateNamedComps} % \\ Eine Funktion die aus einem CircuitDescriptor die Benamte Komponentenliste erstellt
  \item \hsSource{getInPinNames}      % \\ Eine Funktion die eine In-PinID / Namen's Liste aus der NamedComp Liste erzeugt
  \item \hsSource{getOutPinNames}     % \\ Eine Funktion die eine Out-PinID / Namen's Liste aus der NamedComp Liste erzeugt
  \item \hsSource{getInPinName}       % \\ Eien weitere Funktion die ein InPinID/Namens Paar aus der NamedComp Liste und aus einer PinID erstellt
  \item \hsSource{getOutPinName}      % \\ Eien weitere Funktion die ein OutPinID/Namens Paar aus der NamedComp Liste und aus einer PinID erstellt
\end{itemize}


\par 
Die Funktion \hsSource{generateNamedComps} erzeugt die benannten Komponenten Liste. Hier werden nur die Pins benannt. Ein Pin kann immer von
zwei Seiten aus gesehen werden. Es gibt \begriff{externe} Pins, also Pins, die an die von außen ein Draht angeschlossen werden kann. Diese
werden unter dem namen \hsSource{generateSuperNames} zusammengefasst. Daneben existieren auch \begriff{interne} Pins, die man hinter dem
Namen \hsSource{generateSubNames} findet. Diese Unterscheidung muss getroffen werden, da externe Pins mit den Präfixen \hsSource{nameExI}
und \hsSource{nameExO} versehen werden. Interne Pins werden mit \hsSource{nameInI} sowie \hsSource{NameInO} benamt.
\hsSource{generateSuperNames} wird auf den übergebenen \hsSource{CircuitDescriptor} angewandt, die Funktion \hsSource{generateSubNames}
hingegen auf alle dem \hsSource{CircuitDescriptor} untergeordneten \hsSource{CircuitDescriptor}en. 

\begin{code}
  generateNamedComps :: CircuitDescriptor -> [NamedComp]
  generateNamedComps g = generateSuperNames g : (map generateSubNames $ nodes g)
      where generateSuperNames g = ( compID g, ( namePins sinks   nameExI g
                                               , namePins sources nameExO g
                                               )
                                   )
            generateSubNames g   = ( compID g, ( namePins sinks   nameInI g
                                               , namePins sources nameInO g
                                               )
                                   )
\end{code}


\par
Die beiden Funktionen \hsSource{getNamedInPins} sowie \hsSource{getInPinNames} holen jeweils aus einer Liste von \hsSource{NamedComp} und
einer \hsSource{CompID} den passenden Datensatz heraus. Hierbei unterscheiden sie sich lediglich im Rückgabetyp voneinander. So liefert
\hsSource{getNamedInPins} die \hsSource{InNames}, also eine Liste benannter Pins, zurück. \hsSource{getInPinNames} liefert nur eine Liste
der Namen.

\begin{code}
  -- TODO: is fst the right function to get the in-names ???

  getNamedInPins :: [NamedComp] -> CompID -> InNames
  getNamedInPins = getPinNames fst

  getInPinNames :: [NamedComp] -> CompID -> [String]
  getInPinNames cname cid = map snd $ getNamedInPins cname cid
\end{code}


\par 
Die beiden nächsten Funktionen, \hsSource{getNamedOutPins} sowie \hsSource{getOutPinNames} verhalten sich analog zu den beiden
\begriff{InPin} varianten, mit dem Unterschied, dass diese beiden Funktionen sich auf die Ausgabepins beziehen.

\begin{code}
  -- TODO: is snd the right function to get the out-names ???

  getNamedOutPins :: [NamedComp] -> CompID -> OutNames
  getNamedOutPins = getPinNames snd

  getOutPinNames :: [NamedComp] -> CompID -> [String]
  getOutPinNames cname cid = map snd $ getNamedOutPins cname cid
\end{code}


\par 
Mit der Funktion \hsSource{getPinNames} wird die eigentliche Logik beschrieben, die für das herausfiltern der Pin-Namen notwendig ist. Aus
der übergebenen Liste der benannten Komponenten werden all die Komponenten heraus gefiltert, die der ebenfalls übergebenen Komponenten ID
entsprechen. Im nächsten Schritt (\hsSource{map snd}) wird die Komponenten ID verworfen. Die übergeben Funktion \hsSource{f} definiert nun,
ob das Ergebnis eingehende oder ausgehende Pins sind. Der letzte Schritt entfernt überflüssige Listen-Verschachtelungen.

\begin{code}
  getPinNames :: (([NamedPin], [NamedPin]) -> [NamedPin]) -> [NamedComp] -> CompID -> [(PinID, String)]
  getPinNames f cname cid
      = concat
      $ map f
      $ map snd
      $ filter (\(x, _) -> x == cid)
      $ cname
\end{code}


\par
Die folgenden fünf Funktionen arbeiten analog zu den vorhergehenden fünf. Sie unterscheiden sich darin, dass sie einen weiteren Parameter
erwarten, ein \hsSource{PinID}. Dieser Parameter schränkt die Ergebnismenge auf exakt ein Ergebnis ein, sie liefern also einen benannten
Pin.
\begin{code}
  getNamedInPin :: [NamedComp] -> CompID -> PinID -> NamedPin
  getNamedInPin = getPinName getNamedInPins

  getNamedOutPin :: [NamedComp] -> CompID -> PinID -> NamedPin
  getNamedOutPin = getPinName getNamedOutPins

  getInPinName :: [NamedComp] -> CompID -> PinID -> String
  getInPinName cname cid pid = snd $ getNamedInPin cname cid pid

  getOutPinName :: [NamedComp] -> CompID -> PinID -> String
  getOutPinName cname cid pid = snd $ getNamedOutPin cname cid pid

  getPinName :: ([NamedComp] -> CompID -> [NamedPin]) -> [NamedComp] -> CompID -> PinID -> NamedPin
  getPinName f cname cid pid
      = head
      $ filter (\(x, _) -> x == pid)
      $ f cname cid
\end{code}


\par
Die letzte Funktion in diesem Modul erzeugt aus einem \hsSource{CircuitDescriptor} und einem Präfix eine Liste benannter Pins. Diese
Funktion wird in \hsSource{generateNamedComps} verwendet, um interne und externe Pins mit Namen zu versehen. Je nach übergebenen
\hsSource{f} produziert \hsSource{namePins} benannte interne oder benannte externe Pinlisten.
\begin{code}
  namePins :: (CircuitDescriptor -> Pins) -> String -> CircuitDescriptor -> [NamedPin]
  namePins f pre g
      = map (\x -> (x, pre ++ (show x))) $ f g
\end{code} 
