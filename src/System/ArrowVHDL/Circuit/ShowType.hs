-- Das Modul \hsSource{Circuit.ShowType} ist ein strukturelles Modul nach dem Fassaden-Entwurfsmuster. 

module System.ArrowVHDL.Circuit.ShowType (ShowType(..))
where


-- Folgenden Module werden benötigt, um die Instanzen definieren zu können:

import System.ArrowVHDL.Circuit.ShowType.Class (ShowType(..))
import System.ArrowVHDL.Circuit.ShowType.Instance
