-- Das Modul \hsSource{Circuit.Grid} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

module System.ArrowVHDL.Circuit.Grid 
    ( Grid(..)
    , runGrid
    )
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import System.ArrowVHDL.Circuit.Grid.Datatype
import System.ArrowVHDL.Circuit.Grid.Instance
