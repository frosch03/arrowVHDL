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

\section{ShowType Klassendefinition}
\label{mod:Circuit.ShowType.Class}

Das Modul \hsSource{Circuit.ShowType.Class} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise
beschreiben, bearbeiten oder benutzen zu können.

\begin{code}
  module Circuit.ShowType.Class
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Prelude hiding (id, (.))
  import qualified Prelude as Pr
  
  import Control.Category 

  import Circuit.Descriptor
\end{code}


\subsection{Klassendefinition}
Zunächst einmal müssen die Klassen definiert werden. Diese Vorgaben müssen von allen Instanzdefinitionen befolgt werden.

\par
Die \hsSource{ShowType}-Klasse wird benötigt, um zur Laufzeit Typinformationen in den \hsSource{Arrow} zu bekommen. Dies wird immer dann
ausgenutzt, wenn mittels \hsSource{arr} eine Funktion in einen Arrow geliftet wird. Wie man später in der \hsSource{Arrow}-Klassen
Definition erkennen kann, ist \hsSource{arr} nur für Funktionen definiert, die auch in \hsSource{ShowType} sind.

\begin{code}
  class ShowType b c where
    showType :: (b -> c) -> CircuitDescriptor
\end{code}
