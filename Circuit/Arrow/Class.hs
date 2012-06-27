{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}


-- Das Modul \hsSource{Circuit.Arrowdefinition} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise beschreiben,
-- bearbeiten oder benutzen zu können.

module Circuit.Arrow.Class 
    ( Arrow(..)
    , ArrowLoop(..)
    , ArrowCircuit(..)
    , ArrowChoice(..)
    )
where



-- Folgenden Module werden benötigt, um die Arrows definieren zu können:
import Prelude hiding (id, (.))
import qualified Prelude as Pr

import Control.Category 

import Circuit.ShowType



-- Durch die Angabe von Präzedenzen lassen sich Operatoren ohne Klammer schreiben. Dies hilft bei der Leserlichkeit von Quellcode. Folgende
-- Präzedenzen werden festgelegt:


infixr 3 ***
infixr 3 &&&


-- Zunächst einmal müssen die Klassen definiert werden. Diese Vorgaben müssen von allen Instanzdefinitionen befolgt werden.


-- Hier folgt die Klassendefinition eines Arrows. Anzumerken ist, dass diese Klassendefinition geringfügig von der Standard-Definition
-- abweicht. Die Arrow-Klasse wird hier in der \hsSource{arr} Funktion soweit eingeschränkt, dass nur Funktionen (\hsSource{(b -> c)}) von
-- \hsSource{arr} akzeptiert werden, die Mitglied der Typklasse \hsSource{ShowType b c} sind. 


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



-- Weitere Arrow Instanzen sind \hsSource{ArrowLoop}. Hierbei handelt es sich um Arrow, die sich selbst wieder aufrufen können, und somit einen
-- Fixpunkt in der Arrow-Funktion versuchen zu finden. 


class (Arrow a) => ArrowLoop a where
    loop :: a (b,d) (c,d) -> a b c



-- Mit \hsSource{ArrowCircuit} sind die Arrows gemeint, die sich als getaktete Schaltung darstellen lassen. Definiert werden muss hier
-- lediglich, was ein Takt ist.


class (ArrowLoop a) => ArrowCircuit a where
    delay :: b -> a b b



-- Eine weitere Typklasse ist \hsSource{ArrowChoice}. Mit Arrow-Choice ist es Möglich, Varianten der selben Lösung parallel nebeneinander
-- erzeugen zu lassen. 


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

