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

\section{Pfeildefinitionen}
\label{mod:Circuit.Arrow}

Das Modul \hsSource{Circuit.Arrowdefinition} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
bearbeiten oder benutzen zu können.

\begin{code}
  module Circuit.Arrow 
    ( Arrow(..)
    , ArrowLoop(..)
    , ArrowCircuit(..)
    , ArrowChoice(..)
    , returnA
    , movebrc
    , backbrc
    , swapsnd
    ) 
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Prelude (id)

  import Circuit.Arrow.Class
  import Circuit.Arrow.Instance
\end{code}


\par
Neben den Klassendefinitionen ist es ganz Praktisch, einen Arrow zu definieren, der in allen Arrow-Beschreibungen wieder Anwendung finden
wird. Es handelt sich um \hsSource{returnA}, was der Kategorientheoretischen \begriff{Unit} Funktion entspricht.

\begin{code}
  returnA :: (Arrow a) => a b b
  returnA = arr id
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

