\section{ShowType Frontend}
\label{mod:Circuit.ShowType}

Das Modul \hsSource{Circuit.ShowType} ist ein strukturelles Modul nach dem Fassaden-Entwurfsmuster. 

\begin{code}
  module Circuit.ShowType (ShowType(..))
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Instanzen definieren zu können:

\begin{code}
  import Circuit.ShowType.Class (ShowType(..))
  import Circuit.ShowType.Instance
\end{code}
