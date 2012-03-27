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
\label{mod:Grid:Arrowdefinition}

Das Modul \hsSource{Grid.Arrowdefinition} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
bearbeiten oder benutzen zu können.

\begin{code}
  module Grid.Traversal
  where
\end{code}


\par
Folgenden Module werden benötigt, um die Arrows definieren zu können:

\begin{code}
  import Prelude hiding (id, (.))
  import qualified Prelude as Pr
  
  import Control.Category 
  
  import Grid.Core
  import Grid.Auxillary
  import Grid.Graph
  import Grid.Workers (flatten)
\end{code}


\par
Durch die Angabe von Präzedenzen lassen sich Operatoren ohne Klammer schreiben. Dies hilft bei der Leserlichkeit von Quellcode. Folgende
Präzedenzen werden festgelegt:

\begin{code}
  infixr 3 ***
  infixr 3 &&&
\end{code}

\subsection{Klassendefinitionen}
Zunächst einmal müssen die Klassen definiert werden. Diese Vorgaben müssen von allen Instanzdefinitionen befolgt werden.

\par
Die \hsSource{ShowType}-Klasse wird benötigt, um zur Laufzeit Typinformationen in den \hsSource{Arrow} zu bekommen. Dies wird immer dann
ausgenutzt, wenn mittels \hsSource{arr} eine Funktion in einen Arrow geliftet wird. Wie man später in der \hsSource{Arrow}-Klassen
Definition erkennen kann, ist \hsSource{arr} nur für Funktionen definiert, die auch in \hsSource{ShowType} sind.

\begin{code}
  class ShowType b c where
    showType :: (b -> c) -> CircuitDescriptor
\end{code}


\par
Hier folgt die Klassendefinition eines Arrows. Anzumerken ist, dass diese Klassendefinition geringfügig von der Standard-Definition
abweicht. Die Arrow-Klasse wird hier in der \hsSource{arr} Funktion soweit eingeschränkt, dass nur Funktionen (\hsSource{(b -> c)}) von
\hsSource{arr} akzeptiert werden, die Mitglied der Typklasse \hsSource{ShowType b c} sind. 

\begin{code}
  class (Category a) => Arrow a where
    arr    :: (ShowType b c) => (b -> c) -> a b c
    first  :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    second f = arr swap >>> first f >>> arr swap
             where swap :: (b, c) -> (c, b)
                   swap   ~(x, y)  = (y, x)
    (***)  :: a b c -> a b' c' -> a (b, b') (c, c')
    f *** g = first f >>> second g
    (&&&)  :: a b c -> a b c'  -> a b (c, c')
    f &&& g = arr (\b -> (b, b)) >>> f *** g
\end{code}


\par
Weitere Arrow Instanzen sind \hsSource{ArrowLoop}. Hierbei handelt es sich um Arrow, die sich selbst wieder aufrufen können, und somit einen
Fixpunkt in der Arrow-Funktion versuchen zu finden. 

\begin{code}
  class (Arrow a) => ArrowLoop a where
      loop :: a (b,d) (c,d) -> a b c
\end{code}


\par
Mit \hsSource{ArrowCircuit} sind die Arrows gemeint, die sich als getaktete Schaltung darstellen lassen. Definiert werden muss hier
lediglich, was ein Takt ist.

\begin{code}
  class (ArrowLoop a) => ArrowCircuit a where
      delay :: b -> a b b
\end{code}


\par
Eine weitere Typklasse ist \hsSource{ArrowChoice}. Mit Arrow-Choice ist es Möglich, Varianten der selben Lösung parallel nebeneinander
erzeugen zu lassen. 

\begin{code}
  class (Arrow a) => ArrowChoice a where
      left  :: a b c -> a (Either b d) (Either c d) 
      right :: a b c -> a (Either d b) (Either d c) 
      (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
      (|||) :: a b d -> a c  d  -> a (Either b c ) d
      
      right f = arr mirror >>> left f >>> arr mirror
          where mirror :: Either x y -> Either y x
                mirror (Left x)  = (Right x)
                mirror (Right x) = (Left x)
      
      f +++ g = left f  >>> right g
      
      f ||| g = f +++ g >>> arr untag
          where untag (Left x)  = x
                untag (Right x) = x
\end{code}


\par
Neben den Klassendefinitionen ist es ganz Praktisch, einen Arrow zu definieren, der in allen Arrow-Beschreibungen wieder Anwendung finden
wird. Es handelt sich um \hsSource{returnA}, was der Kategorientheoretischen \begriff{Unit} Funktion entspricht.

\begin{code}
  returnA :: (Arrow a) => a b b
  returnA = arr id
\end{code}


\par
Um Funktions-Arrows ausführen zu können, wird eine Applikations-Instanz eines Arrows benötigt. Angegeben ist hier die minimal Definition mit
nur den beiden Funktionen \hsSource{arr} sowie \hsSource{first}. 

\begin{code}
  instance Arrow (->) where
    arr f    = f
    first  f = (\(x, y) -> (f x, y))
\end{code}



\section{ShowType}
Noch befinden sich die Showtype Implementationen nicht in einem eigenen Modul. Daher werden diese solange hier mit beschrieben. Showtype
wird hier für bestimmte Tupel-Typen beschrieben. Einzig die Tupel-Information ist das, was mittels Showtype in Pin-Informationen übersetzt
wird. 


\par
Die \hsSource{ShowType}-Instanz definiert die verschiedenen Typenvarianten, welche abbildbar sind.

\par 
Mit dem Typ \hsSource{(b, c) -> (c, b)} stellt die folgende Variante eine dar, in der die Ein- und Ausgänge miteinander vertauscht werden.

\begin{code}
  instance ShowType (b, c) (c, b) where
    showType _ = emptyCircuit { label = "|b,c>c,b|" 
                            , sinks = mkPins 2
                            , sources = mkPins 2
                            }
\end{code}


\par 
Diese Variante mit dem Typ \hsSource{b -> (b, b)} beschreibt den Fall, in dem ein Eingang verdoppelt wird.
  
\begin{code}
  instance ShowType b (b, b) where
    showType _ = emptyCircuit { label = "|b>b,b|"
                            , sinks = mkPins 1
                            , sources = mkPins 2
                            }
\end{code}
  

\par 
Mit dem Typ \hsSource{(b, b) -> b} wir dann der Fall abgebildet, der zwei Eingänge auf einen zusammenfasst. 

\begin{code}
  instance ShowType (b, b) b where
    showType _ = emptyCircuit { label = "|b,b>b|"
                            , sinks = mkPins 2
                            , sources = mkPins 1
                            }
\end{code}
  
%%% instance ShowType (b, c) (b', c') where
%%%   showType _ = emptyCircuit { label = "b,c>b',c'"
%%%                           , sinks = mkPins 1
%%%                           , sources = mkPins 1
%%%                           }
  
%%%instance ShowType b b where
%%%  showType _ = emptyCircuit { label = "|b>b|"
%%%                          , sinks = mkPins 1
%%%                          , sources = mkPins 1
%%%                          }
  
%%% instance ShowType (b -> (c, d)) where
%%%   showType _ = emptyCircuit { label = "b>c,d"
%%%                           , sinks = mkPins 1
%%%                           , sources = mkPins 1
%%%                           }

%%% instance ShowType ((b, c) -> d) where
%%%   showType _ = emptyCircuit { label = "b,c>d"
%%%                           , sinks = mkPins 1
%%%                           , sources = mkPins 1
%%%                           }
  
\par
Letztlich bleibt noch der allgemeinste Fall der Möglich ist. Diese Varianten ist somit auch eine \begriff{CatchAll} Variante.

\begin{code}
  instance ShowType b c where
    showType _ = emptyCircuit { label = "|b>c|"
                            , sinks = mkPins 1
                            , sources = mkPins 1
                            }
\end{code}

%%% ---------------------------------------------------------------------------------------------------

\section{Grid - Steckbrett}
Ein \hsSource{Grid} ist ein Datentyp, für den die Arrow-Klassen implementiert werden. Man spricht dann von einem \hsSource{Grid}-Arrow. Der
Name \hsSource{Grid} soll dabei an ein Steckbrett erinnern, auf denen man Bauteile anbringen und miteinander verbinden kann. \hsSource{Grid}
besitzt 3 Typvariablen (\hsSource{a}, \hsSource{b}, \hsSource{c}), wobei \hsSource{a} die jeweilige Arrow-Instanz repräsentiert (z.B.
\hsSource{(->)}), \hsSource{b} und \hsSource{c} stellen den Typ des Arrows dar. Die Funktion \hsSource{(+1)} hat den Typ \hsSource{Int ->
Int}. Dies lässt sich auch in präfix-Notation schreiben \hsSource{(->) Int Int} und ist damit analog zu den Typvariablen des \hsSource{Grid}
Types. 

\begin{code}
  newtype Grid a b c = GR (a (b, CircuitDescriptor) (c, CircuitDescriptor))
\end{code}


\subsection{Grid ist eine Kategorie}
Bevor für den Typ \hsSource{Grid} eine Arrow-Instanz implementiert werden kann, muss \hsSource{Grid} Mitglied der Typklasse
\hsSource{Category} sein. 

\begin{code}
  instance (Arrow a) => Category (Grid a) where
    id              
      = GR id

    (GR f) . (GR g) 
      = GR $ proc (x, sg) -> do
                  (x_g, sg_g) <- g -< (x, sg)
                  (x_f, sg_f) <- f -< (x_g, sg   `connect` sg_g)
                  returnA          -< (x_f, sg_g `connect` sg_f)
\end{code}


\par
Im nächsten Schritt wird dann die Arrow-Instanz von \hsSource{Grid} implementiert. Laut Definition ist ein Arrow vollständig definiert durch
die Funktionen \hsSource{arr} und \hsSource{first}. Alle weiteren Funktion lassen sich aus diesen beiden ableiten. Da hier aber die
Kontrolle über die Implementierung jeder Funktion behalten werden soll, ist hier eine Implementation für alle einzel-Funktionen gegeben.

\begin{code}
  instance (Arrow a) => Arrow (Grid a) where
    arr   f       
      = GR $ arr (\(x, sg) -> (f x, showType f))
  
    first  (GR f) 
      = GR $   arr swapsnd 
           >>> first f 
           >>> arr swapsnd 
           >>> second (arr (flip combine idCircuit))
  
    second (GR f) 
      = GR $   arr movebrc
           >>> second f 
           >>> arr backbrc
           >>> second (arr (combine idCircuit))
  
    (GR f) &&& (GR g) 
      = GR $   dup
           >>> first  f
           >>> second g
           >>> dup
           >>> first  (arr (\x -> (,)        (fst.fst$x) (fst.snd$x)))
           >>> second (arr (\s -> dupCombine (snd.fst$s) (snd.snd$s)))
      where dup = arr (\x -> (x, x))
  
    (GR f) *** (GR g) 
      = GR $   dup
           >>> first  (arr (\x -> (,) (fst.fst$x) (snd$x)))
           >>> second (arr (\x -> (,) (snd.fst$x) (snd$x)))
           >>> first  f
           >>> second g
           >>> dup
           >>> first  (arr (\x -> (,)     (fst.fst$x) (fst.snd$x)))
           >>> second (arr (\s -> combine (snd.fst$s) (snd.snd$s)))
      where dup = arr (\x -> (x, x))
\end{code}


%%% \par 
%%% Um den \hsSource{Grid}-Arrow zu \hsSource{ArrowChoice} hinzufüge, so ist die Implementierung von \hsSource{ArrowChoice} für \hsSource{Grid}
%%% notwendig. 
%%% 
%%% \begin{code}
%%%   instance (Arrow a) => ArrowChoice (Grid a) where
%%%       left  f = f      +++ arr id
%%%       right g = arr id +++ g 
%%%       f +++ g = (Left . f) ||| (Right . g)
%%%       f ||| g = either
%%% \end{code}


%%% \par
%%% Die Definition von \hsSource{ArrowLoop} ist dann notwendig, wenn Schleifen abgebildet werden sollen. Hierzu ist die Implementation einer
%%% einzigen Funktion notwendig, nämlich der \hsSource{loop :: a (b, d) (c, d) -> a b c} notwendig.
%%% 
%%% \begin{code}
%%%   instance (ArrowLoop a) => ArrowLoop (Grid a) where
%%%       loop (GR f) = GR (loop (arr swapsnd >>> f >>> arr swapsnd))
%%% \end{code}


\par
Zu dem \hsSource{Grid}-Arrow gehört außerdem noch eine Funktion, die den \hsSource{Grid}-Typ auspacken kann und dann ``ausführen'' kann.

\begin{code}
  runGrid :: (Arrow a) => Grid a b c -> a (b, CircuitDescriptor) (c, CircuitDescriptor)
  runGrid (GR f) = f
\end{code}



\section{Ströme}
In Schaltkreisen sind Schleifen nicht über den normalen \hsSource{ArrowLoop} Ansatz realisierbar. Das Problem liegt darin, dass zwischen
zwei Berechnungen keine Verzögerung stattfindet. Wollte man das erreichen, so benötigt man mindestens \begriff{Register}, die einen
Taktzyklus verzögern. Damit ist dann festgelegt, dass im Grunde Schleifen in Hardware nur dann Sinn ergeben, wenn ein kontinuierlicher
Datenstrom verarbeitet werden kann. 

\par
Auch für andere Ansätze wird eine \hsSource{Stream}-Arrow notwendig. Dies kann beispielsweise der Fall sein falls Zwischenergebnisse
Innerhalb des Schaltkreises ermittelt werden sollen. 

\par
Zunächst wird der Datentyp definiert. \hsSource{Stream} hat einen Typkonstruktor \hsSource{SF} und besteht aus einer Funktion, welche aus
einer Liste von b's (\hsSource{[b]}) eine Liste von c's \hsSource{[c]}) erzeugt. 

\begin{code}
  newtype Stream b c = SF { runStream :: ([b] -> [c]) }
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



\section{Arrow Helfer}
Typischerweise verwendet man den Begriff \begriff{Synthese} in der Hardware-Community für den Prozess, aus einer Modellhaften
Hardwarebeschreibung heraus tatsächlichen Hardwarecode (beispielsweise VHDL) zu erzeugen. Daneben ist auch die \begriff{Simulation} von
Hardwaremodellen notwendig, um die entworfenen Modelle vor der Realisierung überprüfen zu können.

\par
Die beiden Prozesse lassen sich auch auf das \hsSource{Grid}-Arrow Modell übertragen. So stellt \hsSource{synthesize} eine Funktion dar, die
aus einem gegebenen \hsSource{Grid} die fertige Hardwarebeschreibung \footnote{in diesem Fall ausgeliefert in VHDL} erzeugt. Die Simulation
wird mittels der Funktion \hsSource{simulate} abgebildet. Diese Funktion erzeugt nun aus einem \hsSource{Grid}-Arrow ein
\hsSource{Stream}-Arrow, der dann mit einem kontinuierlichem Datenstrom simuliert werden kann.

\begin{code}
  synthesize :: Grid (->) b c -> CircuitDescriptor
  synthesize f = flatten $ snd $ runGrid f (undefined, NoDescriptor)
  
  simulate :: Grid (->) b c -> Stream b c 
  simulate f = arr (toFunctionModel f)
\end{code}

\par
Um einen \hsSource{Grid}-Arrow kombinatorisch auszuwerten, existiert die Hilfsfunktion \hsSource{toFunctionModel}.
  
\begin{code}
  toFunctionModel :: Grid (->) b c -> (b -> c)
  toFunctionModel f = \x -> fst $ runGrid f (x, NoDescriptor)
\end{code}
  

%%% TODO : Ist synthesize mit unit () möglich? 
%%% TODO : Keine zufälligen daten ... 
%%% TODO : Frage nach simulate / synthesize => right ... 
%%% TODO : _kritische_pfad_analyse_ / ... 



\par 
Weitere Hilfsfunktionen werden notwendig, um schon bestehende \hsSource{Grid}-Arrows mit Schaltkreis Beschreibungen anzureichern. 

\begin{code}
  insert :: b -> (a, b) -> (a, b)
  insert sg ~(x, _) = (x, sg)
  
  insEmpty = insert emptyCircuit { label = "eeeempty", sinks = mkPins 1, sources = mkPins 3 }
  
  augment :: (Arrow a) => CircuitDescriptor -> Grid a b c -> Grid a b c
  augment sg (GR f) = GR $ f >>> arr (insert sg)
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
