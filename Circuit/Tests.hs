-- Dieses Modul beschreibt eine Reihe von Funktionen zum prüfen von Eigenschaften. Diese Funktionen haben eine Gemeinsamkeit, nämlich dass sie
-- letztlich immer einen \hsSource{Bool}-Wert erzeugen. 


module Circuit.Tests 
where

-- Lediglich das Modul \hsSource{Circuit.Descriptor} wird benötigt


import Circuit.Descriptor


-- Die Funktion \hsSource{isFromOrToComp} ermittelt ob eine übergebene Kante eine Verbindung zu einer Komponente, die über ihre Komponenten ID
-- identifiziert wird, darstellt.

isFromOrToComp :: CompID -> Edge -> Bool
isFromOrToComp cid (MkEdge (Nothing, pi) (Just co, po)) = cid == co
isFromOrToComp cid (MkEdge (Just ci, pi) (Nothing, po)) = cid == ci
isFromOrToComp cid (MkEdge (Just ci, pi) (Just co, po)) = cid == co 


-- Jeder Schaltkreis besitzt eine innere Verschaltung und daneben eine Verbindung zur Außenwelt. Eine Kante kann von ``Außen'' her kommen, oder
-- nach ``Außen'' gehen. Genau diese Kanten sind die Pins eines tatsächlichen Chips. 


-- Mit \hsSource{isToOuter} wird getestet, ob es sich um eine abgehende Kante, also einen ``outgoing''-Pin handelt.

isToOuter :: Edge -> Bool
isToOuter (MkEdge (_, _) (Nothing, _)) = True
isToOuter _                            = False


-- \par Das Pendant zu abgehenden Kanten sind eingehende Kanten. Diese werden auch als Inputs bezeichnet. \hsSource{isFromOuter} testet, ob
-- eine Kante eingehend ist.

isFromOuter :: Edge -> Bool
isFromOuter (MkEdge (Nothing, _) (_, _)) = True
isFromOuter _                            = False




-- Mit der Funktion \hsSource{hasLabel} wird überprüft, ob ein Schaltkreis den übergebenen Namen trägt.

hasLabel :: String -> CircuitDescriptor -> Bool
hasLabel s
    = ((== s).label.nodeDesc)


-- \hsSource{isAtomic} überprüft, ob ein Baustein atomar ist, also ob er aus weiteren Bausteinen zusammengesetzt ist, oder nicht.

isAtomic :: CircuitDescriptor -> Bool
isAtomic g
    = if (length (nodes g) == 0) then True else False


-- Die Funktion \hsSource{isSuperNodeOf} testet, ob eine Komponente eine bestimmte andere Komponente, identifiziert durch ihre Komponenten ID,
-- enthält.

isSuperNodeOf :: CircuitDescriptor -> CompID -> Bool
isSuperNodeOf g cid 
    = if length (filter (== cid) subNodes) > 0
        then True
        else False
    where subNodes = map (nodeId.nodeDesc) $ nodes g


-- Mit der Funktion \hsSource{isGenerated} lässt sich herausfinden, ob einen Komponente eine vom Entwickler entwickelte Komponente ist, oder ob
-- diese Komponente automatisch vom System erzeugt wurde.

isGenerated :: CircuitDescriptor -> Bool
isGenerated s = ((== '|').head.label.nodeDesc) s && ((== '|').head.reverse.label.nodeDesc) s



-- %%% TODO : isID ist nur eine debugging funktion und muss rausgenommen werden

isID :: CircuitDescriptor -> Bool
isID = hasLabel "-ID-"

-- In einem weiteren Test wird die Art der Schaltung überprüft. Handelt es sich bei dem vorliegenden \hsSource{CircuitDescriptor} um einen
-- Kombinatorischen Schaltkreis, um ein Register oder um eine Schleife?

isCombinatorial :: CircuitDescriptor -> Bool
isCombinatorial (MkCombinatorial _ _ _ _ _) = True
isCombinatorial otherwise           = False

isRegister :: CircuitDescriptor -> Bool
isRegister (MkRegister _ _) = True
isRegister otherwise      = False

isLoop :: CircuitDescriptor -> Bool
isLoop (MkLoop _ _ _ _ _) = True
isLoop otherwise          = False
