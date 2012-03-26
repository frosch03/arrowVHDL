\section{Zusammengefüge}
\label{mod:Grid:Splice}


Das Module \hsSource{Grid.Splice} bietet nach außen hin nur eine Funktion an, nämlich \hsSource{splice}. Diese Funktion führt zwei
Schaltungen zu einer neuen zusammen. Dabei ist noch nicht festgelegt, wie dieses Zusammenführen tatsächlich aussieht. 

\begin{code}
  module Grid.Splice
  where
\end{code} 

\par
Verwendet werden die Standard-Definitionen, sowie eine Sensor und einer Worker Funktion.


\begin{code}
  import Grid.Core
  import Grid.Workers (alterCompIDs)
  import Grid.Sensors (maxCompID)
\end{code} 


\par
Auch wenn hier tatsächlich zwei Funktionen stehen wird \hsSource{splice} doch als eine Einheit angesehen. \hsSource{splice'} enthält die
Funktionalität, \hsSource{splice} ist der öffentliche Bezeichner, der obendrein noch eine grundlegende Fehlerprüfung macht.

\par
\hsSource{splice} wird eine \hsSource{rewire} Funktion übergeben. Diese Funktion enthält die Logik, nach der die ``Verdrahtung'' der beiden
Schaltkreise erfolgen wird. Hier ist es dann möglich beispielsweise sequentiell oder parallel zu verdrahten. Außerdem erwartet
\hsSource{splice} noch zwei Schaltungen, die zusammengeführt werden sollen. Diese beiden werden dann auf die gewählte Art miteinander
verbunden. Die übrigen ``Drähte'' werden nach außen geführt, ein neuer Name wird erzeugt und dieser neue Schaltkreis wird dann
zurückgegeben.

\begin{code}
  splice :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  splice _           sg NoDescriptor = sg
  splice _           NoDescriptor sg = sg
  splice (rewire, s) sg_f sg_g       = splice' (rewire, s) sg_f sg_g


  splice' :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
  splice' (rewire, s) sg_f sg_g 
      = MkDescriptor 
             { label   = (label sg_f') ++ s ++ (label sg_g')
             , compID  = 0
             , nodes   = sg_f': sg_g' : []
             , edges   = es
             , sinks   = srcs 
             , sources = snks
             }
      where sg_f'              = alterCompIDs 1                    sg_f
            sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
            (es, (srcs, snks)) = rewire sg_f' sg_g'
\end{code} 
