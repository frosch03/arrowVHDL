-- Das Modul \hsSource{Circuit.Stream} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

module System.ArrowVHDL.Circuit.Stream
    ( Stream(..)
    )
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import System.ArrowVHDL.Circuit.Stream.Datatype
import System.ArrowVHDL.Circuit.Stream.Instance
