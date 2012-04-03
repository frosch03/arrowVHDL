\section{Grid Datentyp}
\label{mod:Circuit.Grid.Datatype}

Das Modul \hsSource{Circuit.Arrowdefinition} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
bearbeiten oder benutzen zu können.

\begin{code}
  module Circuit.Grid.Datatype
  where
\end{code}

\par
Folgenden Module werden benötigt, um den Datentyp definieren zu können:

\begin{code}
  import Circuit.Descriptor
\end{code} 


\par
Ein \hsSource{Grid} ist ein Datentyp, für den die Arrow-Klassen implementiert werden. Man spricht dann von einem \hsSource{Grid}-Arrow. Der
Name \hsSource{Grid} soll dabei an ein Steckbrett erinnern, auf denen man Bauteile anbringen und miteinander verbinden kann. \hsSource{Grid}
besitzt 3 Typvariablen (\hsSource{a}, \hsSource{b}, \hsSource{c}), wobei \hsSource{a} die jeweilige Arrow-Instanz repräsentiert (z.B.
\hsSource{(->)}), \hsSource{b} und \hsSource{c} stellen den Typ des Arrows dar. Die Funktion \hsSource{(+1)} hat den Typ \hsSource{Int ->
Int}. Dies lässt sich auch in präfix-Notation schreiben \hsSource{(->) Int Int} und ist damit analog zu den Typvariablen des \hsSource{Grid}
Types. 

\begin{code}
  newtype Grid a b c = GR (a b c, CircuitDescriptor)
\end{code}
