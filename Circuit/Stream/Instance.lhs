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

\section{Stream Instanzen}
\label{mod:Circuit.Stream.Instance}

Das Modul \hsSource{Circuit.Stream.Instance} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
bearbeiten oder benutzen zu können.

\begin{code}
  module Circuit.Stream.Instance
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Prelude hiding (id, (.))
  import qualified Prelude as Pr
  
  import Control.Category 

  import Circuit.Arrow
  
  import Circuit.Descriptor
  import Circuit.Graphs
  import Circuit.Workers (flatten)

  import Circuit.Stream.Datatype
\end{code}



\par 
Im nächsten Schritt wird \hsSource{Stream} dann zu einer Kategorie ernannt, indem die \hsSource{Category}-Typklasse für \hsSource{Stream}
implementiert wird. Erst wenn \hsSource{Stream} eine Kategorie ist, lässt sich \hsSource{Stream} in einen Arrow befördern.

\begin{code}
  instance Category Stream where
    id              
      = SF (id)

    (SF f) . (SF g) 
      = SF (f . g) 
\end{code}


\par
Nachdem \hsSource{Stream} eine Kategorie ist, kann \hsSource{Stream} als Arrow implementiert werden. Ähnlich wie bei der Implementierung der
Arrow Instanz von \hsSource{Grid} ist es auch bei \hsSource{Stream} notwendig, alle Funktionsbeschreibungen anzugeben. Die abgeleiteten
Funktionen der minimal Definition, reichen nicht aus. 

\begin{code}
  instance Arrow Stream where
    arr f             
      = SF $ map f -- ?? (SF . map) f ??

    first  (SF f)     
      = SF $ (uncurry zip) . (\(bs, cs) -> (f bs, cs)) . unzip 

    second (SF g)     
      = SF $ (uncurry zip) . (\(bs, cs) -> (bs,  g cs)) . unzip 

    (SF f) *** (SF g) 
      = SF $ (uncurry zip) . (\(bs, cs) -> (f bs, g cs)) . unzip
\end{code}


\par
Da \hsSource{Stream} ein Arrow ist und Kontinuierliche Datenströme für das Looping notwendig sind, kann für \hsSource{Stream} auch die
\hsSource{ArrowLoop} Instanz angegeben werden.

\begin{code}
  instance ArrowLoop Stream where
    loop (SF f) 
      = SF $ (\bs -> 
               let (cs, ds) = unzip . f $ zip bs (stream ds) 
               in  cs
             )
      where stream ~(x:xs) = x:stream xs
\end{code}


\par
Mit der \hsSource{Stream} Instanz von \hsSource{ArrowLoop} ist es nun auch möglich, die \hsSource{ArrowCircuit} Instanz zu implementieren.
Diese ist eine direkte Umsetzung des \hsSource{delay}'s auf die Listenfunktionalität. 

\begin{code}
  instance ArrowCircuit Stream where
    delay x = SF (x:)
\end{code}
