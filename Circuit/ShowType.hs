-- Das Modul \hsSource{Circuit.ShowType} ist ein strukturelles Modul nach dem Fassaden-Entwurfsmuster. 

module Circuit.ShowType (ShowType(..))
where


-- Folgenden Module werden benötigt, um die Instanzen definieren zu können:

import Circuit.ShowType.Class (ShowType(..))
import Circuit.ShowType.Instance
