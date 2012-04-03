\section{Arrow Hilfsfunktionen}
\label{mod:Circuit.Arrow.Helpers}

Das Modul \hsSource{Circuit.Arrow.Helpers} beschreibt Hilfsfunktionen, zur Arrow-Gestaltung.

\begin{code}
  module Circuit.Helpers
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Circuit.Descriptor
  import Circuit.Grid
  import Circuit.Stream
\end{code}



\section{Arrow Helfer}
Weitere Hilfsfunktionen werden notwendig, um schon bestehende \hsSource{Grid}-Arrows mit Schaltkreis Beschreibungen anzureichern. 

\begin{code}
  insert :: b -> (a, b) -> (a, b)
  insert sg ~(x, _) = (x, sg)
  
  insEmpty = insert emptyCircuit { label = "eeeempty", sinks = mkPins 1, sources = mkPins 3 }
  
  augment :: (Arrow a) => CircuitDescriptor -> Grid a b c -> Grid a b c
  augment sg (GR f) = GR $ f >>> arr (insert sg)
\end{code}


\par
Zu guter letzt werden noch Funktionen benötigt, die bei der Umstrukturierung von Daten gebraucht werden.

\begin{code}
  movebrc :: ((a, b), c) -> (a, (b, c))
  movebrc ~(~(x, y), sg) = (x, (y, sg))
  
  backbrc :: (a, (b, c)) -> ((a, b), c)
  backbrc ~(x, ~(y, sg)) = ((x, y), sg)
  
  swapsnd :: ((a, b), c) -> ((a, c), b)
  swapsnd ~(~(x, y), sg) = ((x, sg), y)
\end{code}

