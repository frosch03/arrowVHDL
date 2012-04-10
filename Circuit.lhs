\section{Modul Circuit}
\label{mod:Circuit}

Das Modul \hsSource{Circuit} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

\begin{code}
  module Circuit
    ( Grid(..)
    , Stream(..)
    , Show(..)
    )
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Circuit.Grid
  import Circuit.Stream
  import Circuit.Show
\end{code}
