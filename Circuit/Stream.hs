-- Das Modul \hsSource{Circuit.Stream} stellt ein Wrapper-Modul nach dem Fassaden Entwurfsmuster dar.

module Circuit.Stream
    ( Stream(..)
    )
where


-- Folgenden Module werden benötigt, um die Arrows definieren zu können:

import Circuit.Stream.Datatype
import Circuit.Stream.Instance
