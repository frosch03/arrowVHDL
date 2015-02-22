{-# LANGUAGE Arrows,
             OverlappingInstances, 
             UndecidableInstances,
             IncoherentInstances,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             FlexibleInstances,
             RebindableSyntax #-}

-- Das Modul \hsSource{Circuit.ShowType.Class} beschreibt, wie die Arrow-Klasse zu implementieren sind um damit später Schaltkreise
-- beschreiben, bearbeiten oder benutzen zu können.

module System.ArrowVHDL.Circuit.ShowType.Class
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import Prelude hiding (id, (.))
import qualified Prelude as Pr

import Control.Category 

import System.ArrowVHDL.Circuit.Descriptor


-- Zunächst einmal müssen die Klassen definiert werden. Diese Vorgaben müssen von allen Instanzdefinitionen befolgt werden.

-- Die \hsSource{ShowType}-Klasse wird benötigt, um zur Laufzeit Typinformationen in den \hsSource{Arrow} zu bekommen. Dies wird immer dann
-- ausgenutzt, wenn mittels \hsSource{arr} eine Funktion in einen Arrow geliftet wird. Wie man später in der \hsSource{Arrow}-Klassen
-- Definition erkennen kann, ist \hsSource{arr} nur für Funktionen definiert, die auch in \hsSource{ShowType} sind.

class ShowType b c where
    showType :: (b -> c) -> CircuitDescriptor
