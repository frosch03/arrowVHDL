{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}


-- Das Modul \hsSource{Circuit.Arrowdefinition} beschreibt, wie die
-- Arrow-Klasse zu implementieren sind um damit später Schaltkreise
-- beschreiben, bearbeiten oder benutzen zu können.


module System.ArrowVHDL.Circuit.Arrow.Instance
where



-- Folgenden Module werden benötigt, um die Arrows definieren zu
-- können:
import System.ArrowVHDL.Circuit.Arrow.Class



-- Da die folgende Arrow-Instanz nicht zur Ausführung benötigt wird,
-- sondern damit das Typsystem an anderer Stelle den richtigen Typ
-- ableiten kann, ist es möglich diese Instanz als \begriff{dummy
-- Instance} zu definieren. Dies bedeutet, dass die Instanz keine
-- definierten Methoden besitzt. Der Kompiler warnt die nicht
-- vorhandenen Methoden zwar an, es bleibt allerdings bei der Warnung.


instance Arrow (->) where
    arr   f = f
    first f = (\(x, y) -> (f x, y))

-- Im folgenden wird eine Instanz der \hsSource{ArrowLoop}-Klasse für
-- einfache Funktionsauswertung definiert. Auch hier reicht eine
-- \begriff{dummy}-Definition für das Typsystem.

instance ArrowLoop (->) where
--    loop f b = let (c, d) = f (b, d) in c
 

