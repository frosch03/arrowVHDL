-- Das Module \hsSource{Circuit.Splice} bietet nach außen hin nur eine
-- Funktion an, nämlich \hsSource{splice}. Diese Funktion führt zwei
-- Schaltungen zu einer neuen zusammen. Dabei ist noch nicht
-- festgelegt, wie dieses Zusammenführen tatsächlich aussieht.

module System.ArrowVHDL.Circuit.Splice
where

-- Verwendet werden die Standard-Definitionen, sowie eine Sensor und
-- einer Worker Funktion.


import Data.List (nub)

import System.ArrowVHDL.Circuit.Graphs

import System.ArrowVHDL.Circuit.Descriptor
import System.ArrowVHDL.Circuit.Workers (alterCompIDs)
import System.ArrowVHDL.Circuit.Sensors (maxCompID)


-- Auch wenn hier tatsächlich zwei Funktionen stehen wird
-- \hsSource{splice} doch als eine Einheit
-- angesehen. \hsSource{splice'} enthält die Funktionalität,
-- \hsSource{splice} ist der öffentliche Bezeichner, der obendrein
-- noch eine grundlegende Fehlerprüfung macht.

-- \hsSource{splice} wird eine \hsSource{rewire} Funktion
-- übergeben. Diese Funktion enthält die Logik, nach der die
-- ``Verdrahtung'' der beiden Schaltkreise erfolgen wird. Hier ist es
-- dann möglich beispielsweise sequentiell oder parallel zu
-- verdrahten. Außerdem erwartet \hsSource{splice} noch zwei
-- Schaltungen, die zusammengeführt werden sollen. Diese beiden werden
-- dann auf die gewählte Art miteinander verbunden. Die übrigen
-- ``Drähte'' werden nach außen geführt, ein neuer Name wird erzeugt
-- und dieser neue Schaltkreis wird dann zurückgegeben.

splice :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) 
       -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
splice _           sg NoDescriptor = sg
splice _           NoDescriptor sg = sg
splice (rewire, s) cd_f cd_g       = splice' (rewire, s) cd_f cd_g


splice' :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) 
        -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
splice' (rewire, s) cd_f cd_g 
    = MkCombinatorial
        { nodeDesc = MkNode 
            { label   = (label.nodeDesc $ cd_f') ++ s ++ (label.nodeDesc $ cd_g')
            , nodeId  = 0
            , sinks   = srcs 
            , sources = snks
            }
        , nodes   = cd_f': cd_g' : []
        , edges   = es
        , cycles  = (cycles cd_f) + (cycles cd_g)
        , space   = (space  cd_f) + (space  cd_g)
        }
    where cd_f'              = alterCompIDs 1                    cd_f
          cd_g'              = alterCompIDs (maxCompID cd_f' +1) cd_g
          (es, (srcs, snks)) = rewire cd_f' cd_g'



-- \subsection{Verdrahtungsvarianten} Die Funktion \hsSource{splice}
-- aus dem Modul \ref{mod:Circuit.Splice} verwendet für das
-- ``verdrahten'' eine der folgenden
-- \hsSource{rewire}-Funktionen. Daneben wird noch eine Zeichenkette
-- zugeordnet, um später debug-Ausgaben erzeugen zu können.

-- Die \hsSource{connect} Funktion verbindet zwei Schaltkreise zu
-- einem neuen. Hierbei wird sequentiell verbunden, als Zeichenkette
-- wird der selbe Operator angegeben, wie er auch aus der
-- \hsSource{Arrow}-Schreibweise schon bekannt ist.

connect :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
connect = splice (seqRewire, ">>>")


-- Neben dem sequentiellen verbinden lassen sich Schaltkreise auch
-- parallel verbinden. Dies ist mit der Funktion \hsSource{combine}
-- möglich.  Als Zeichenkette wird auch hier das aus der
-- \hsSource{Arrow}-Schreibweise bekannte Operator-Symbol verwendet.
-- %%% TODO : combine = frame???

combine :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
combine = splice (parRewire, "&&&")


-- Eine Variante der \hsSource{combine} Funktion ist die Funktion
-- \hsSource{dupCombine}. Hier werden die Eingänge zunächst dupliziert
-- und dann parallel weiter verbunden.

dupCombine :: CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
dupCombine = splice (dupParRewire, ">2>")


-- Um eine Verzögerung in Hardware zu realisieren ist es notwendig die
-- Daten zu speichern. Dies wird über ein Register erreicht. Nach der
-- gewünschten Anzahl von Zyklen, kann dann das Datum aus dem Register
-- wieder ausgelesen werden, und in der Schaltung weiter verwendet
-- werden.  Die Funktion %%%%%%%%%%%%%%%%%%% %%% TODO : Schaltwerke

delayByRegister :: CircuitDescriptor -> CircuitDescriptor
delayByRegister cd@(MkCombinatorial nd _ _ _ _)
    = MkComposite (cd : reg : [])
    where reg = mkRegister nd



-- Möchte man einen \begriff{Loop} erstellen, so wird dieser durch ein
-- Register geführt, dass eine Verzögerung um einen Takt
-- ermöglicht. Die Funktion, die ein Bauteil um eine Schleife mit
-- Register erweitert, nennt sich \hsSource{registerloopRewire}. Diese
-- Funktion lässt sich mittels \hsSource{splice} zu der nach Außen
-- verwendeten \hsSource{loopWithRegister} Funktion umbauen.


loopWithRegister :: CircuitDescriptor -> CircuitDescriptor
loopWithRegister cd 
    = MkLoop
        { nodeDesc = MkNode
            { label   = "loop(" ++ (label.nodeDesc $ cd) ++ ")"
            , nodeId  = 0
            , sinks   = srcs
            , sources = snks
            }
        , nodes   = [alterCompIDs 1 cd]
        , edges   = es
        , space   = space cd
        }
    where (es, (srcs, snks)) = registerLoopRewire cd


-- Unter den \hsSource{rewire}-Funktionen sind Funktionen zu
-- verstehen, die eine Vorstufe für die eigentliche Verbindung (das
-- \begriff{splicen}) darstellen. Zwei Schaltkreise werden jeweils in
-- eine Zwischendarstellung überführt. Die Zwischendarstellung besteht
-- aus einer Liste von neuen Kanten (\hsSource{[Edge]}), zusammen mit
-- den überbleibenden Ein- und Ausgangspins.
-- 
-- Alle \hsSource{rewire}-Funktionen nutzen eine Funktion, nämlich
-- \hsSource{wire}. Das verbinden von Drähten mit Komponenten ist,
-- unabhängig davon ob sequentiell oder parallel verbunden werden
-- soll, immer gleich. Eingehende Parameter zu \hsSource{wire} sind
-- die beiden Komponenten Nummern, sowie die Pin-Listen. Auch diese
-- Funktion erzeugt die schon beschriebene Zwischendarstellung.

wire :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> ([Edge], (Pins, Pins))
wire cid_l cid_r pins_l pins_r 
    = (edges, (drop cnt pins_l, drop cnt pins_r))
    where points_l = map ((,) (cid_l)) pins_l
          points_r = map ((,) (cid_r)) pins_r
          edges    = map (uncurry MkEdge) $ zip points_l points_r
          cnt      = length edges
  

-- \hsSource{wire_} ist ein Synonym für \hsSource{fst . wire}.

wire_ :: Maybe CompID -> Maybe CompID -> Pins -> Pins -> [Edge]
wire_ cid_l cid_r pins_l pins_r = fst $ wire cid_l cid_r pins_l pins_r


-- Bei der Funktion \hsSource{seqRewire} werden die Verbindungen
-- sequentiell erstellt; übrige Ein oder Ausgänge werden zu den gesamt
-- Ein und Ausgängen hinzugefügt.

seqRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
seqRewire sg_l sg_r
  = ( fromOuterToL ++ fromOuterToR ++ edgs ++ fromRToOuter ++ fromLToOuter
      , (super_srcs, super_snks)
      )
    where (edgs, (srcs_l', snks_r')) =  wire (Just $ nodeId.nodeDesc $ sg_l) (Just $ nodeId.nodeDesc $ sg_r) (sources.nodeDesc $ sg_l) (sinks.nodeDesc $ sg_r)
          super_srcs                 =  [0..(length.sinks.nodeDesc   $ sg_l) + length snks_r' -1]
          super_snks                 =  [0..(length.sources.nodeDesc $ sg_r) + length srcs_l' -1]
          ( fromOuterToL, (super_srcs', _)) =  wire Nothing (Just $ nodeId.nodeDesc $ sg_l) super_srcs  (sinks.nodeDesc $ sg_l)
          ( fromOuterToR, (_          , _)) =  wire Nothing (Just $ nodeId.nodeDesc $ sg_r) super_srcs' (drop (length fromOuterToL) $ sinks.nodeDesc $ sg_r)
          ( fromRToOuter, (_, super_snks')) =  wire (Just $ nodeId.nodeDesc $ sg_r) Nothing (sources.nodeDesc $ sg_r) super_snks
          ( fromLToOuter, (_, _))           =  wire (Just $ nodeId.nodeDesc $ sg_l) Nothing (drop (length fromRToOuter) $ sources.nodeDesc $ sg_l) super_snks'


-- Bei der \hsSource{parRewire} Funktion werden beide Bausteine
-- ``übereinander'' angeordnet. Die Eingänge beider Komponenten, sowie
-- deren Ausgänge werden parallel geschaltet.

parRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
parRewire sg_u sg_d
    = ( goingIn_edges ++ goingOut_edges
      , (super_srcs, super_snks)
      )
    where super_srcs = [0..(length $ (sinks.nodeDesc $   sg_u) ++ (sinks.nodeDesc $   sg_d)) -1]
          super_snks = [0..(length $ (sources.nodeDesc $ sg_u) ++ (sources.nodeDesc $ sg_d)) -1]
          goingIn_edges  =  (wire_ Nothing (Just $ nodeId.nodeDesc $ sg_u)                            (super_srcs) (sinks.nodeDesc $ sg_u))
                         ++ (wire_ Nothing (Just $ nodeId.nodeDesc $ sg_d) (drop (length.sinks.nodeDesc $ sg_u) super_srcs) (sinks.nodeDesc $ sg_d))
          goingOut_edges =  (wire_ (Just $ nodeId.nodeDesc $ sg_u) Nothing (sources.nodeDesc $ sg_u)                              (super_snks))
                         ++ (wire_ (Just $ nodeId.nodeDesc $ sg_d) Nothing (sources.nodeDesc $ sg_d) (drop (length.sources.nodeDesc $ sg_u) super_snks))


-- Die Funktion \hsSource{dupParRewire} funktioniert dabei analog zur
-- Funktion \hsSource{parRewire}. Lediglich die Eingänge werden
-- zunächst dupliziert und dann auf beide Komponenten geschaltet.

dupParRewire :: CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))
dupParRewire sg_u sg_d
    = ( goingIn_edges ++ goingOut_edges
      , (super_srcs, super_snks)
      )
    where super_srcs = [0..(length.sinks.nodeDesc $ sg_u) -1]
          super_snks = [0..(length $ (sources.nodeDesc $ sg_u) ++ (sources.nodeDesc $ sg_d)) -1]
          goingIn_edges  =  (wire_ Nothing (Just $ nodeId.nodeDesc $ sg_u) super_srcs (sinks.nodeDesc $ sg_u))
                         ++ (wire_ Nothing (Just $ nodeId.nodeDesc $ sg_d) super_srcs (sinks.nodeDesc $ sg_d))
          goingOut_edges =  (wire_ (Just $ nodeId.nodeDesc $ sg_u) Nothing (sources.nodeDesc $ sg_u)                              (super_snks))
                         ++ (wire_ (Just $ nodeId.nodeDesc $ sg_d) Nothing (sources.nodeDesc $ sg_d) (drop (length.sources.nodeDesc $ sg_u) super_snks))



registerLoopRewire :: CircuitDescriptor -> ([Edge], (Pins, Pins))
registerLoopRewire cd
    = (es, (srcs, snks))
    where reg = mkRegister $ nodeDesc emptyCircuit
          (es1, (srcs1, snks1)) = seqRewire cd reg
          (es2, (srcs2, snks2)) = seqRewire reg cd
          es   = es1 ++ es2
          srcs = nub $ filter (flip elem srcs2) srcs1 ++ filter (flip elem srcs1) srcs2
          snks = nub $ filter (flip elem snks2) snks1 ++ filter (flip elem snks1) snks2 
