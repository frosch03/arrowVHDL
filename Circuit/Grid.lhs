\section{Grid Datentyp}
\label{mod:Circuit.Grid}

Das Modul \hsSource{Circuit.Grid} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

\begin{code}
  module Circuit.Grid 
    ( Grid(..)
    , runGrid
    )
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Circuit.Grid.Datatype
  import Circuit.Grid.Instance
\end{code}
