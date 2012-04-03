\section{Stream Datentyp}
\label{mod:Circuit.Stream}

Das Modul \hsSource{Circuit.Stream} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

\begin{code}
  module Circuit.Stream
    ( Stream(..)
    )
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Circuit.Stream.Datatype
  import Circuit.Stream.Instance
\end{code}
