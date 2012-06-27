-- Das Modul \hsSource{Circuit.Grid} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

module Circuit.Grid 
    ( Grid(..)
    , runGrid
    )
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import Circuit.Grid.Datatype
import Circuit.Grid.Instance
