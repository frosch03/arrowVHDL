\long\def\ignore#1{}

\ignore{
\begin{code}
  {-# LANGUAGE Arrows,
               OverlappingInstances, 
               UndecidableInstances,
               IncoherentInstances,
               NoMonomorphismRestriction,
               MultiParamTypeClasses,
               FlexibleInstances,
               RebindableSyntax #-}
\end{code}
}

\section{Arrow Standard Instanzen}
\label{mod:Circuit.Arrow.Instance}

Das Modul \hsSource{Circuit.Arrowdefinition} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
bearbeiten oder benutzen zu können.

\begin{code}
  module Circuit.Arrow.Instance
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Circuit.Arrow.Class
\end{code}


\par
Da die folgende Arrow-Instanz nicht zur Ausführung benötigt wird, sondern damit das Typsystem an anderer Stelle den richtigen Typ ableiten
kann, ist es möglich diese Instanz als \begriff{dummy Instance} zu definieren. Dies bedeutet, dass die Instanz keine definierten Methoden
besitzt. Der Kompiler warnt die nicht vorhandenen Methoden zwar an, es bleibt allerdings bei der Warnung. 

\begin{code}
  instance Arrow (->) where
--    arr   f = f
--    first f = (\(x, y) -> (f x, y))
\end{code}


\par 
Im folgenden wird eine Instanz der \hsSource{ArrowLoop}-Klasse für einfache Funktionsauswertung definiert. Auch hier reicht eine
\begriff{dummy}-Definition für das Typsystem. 


\begin{code}
  instance ArrowLoop (->) where
--    loop f b = let (c, d) = f (b, d) in c
\end{code} 

