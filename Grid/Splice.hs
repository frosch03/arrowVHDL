module Grid.Splice
where

import Grid.Core
import Grid.Workers (alterCompIDs)
import Grid.Sensors (maxCompID)

splice :: ((Circuit -> Circuit -> ([Edge], (Pins, Pins))), String) -> Circuit -> Circuit -> Circuit
splice _           sg NoSG   = sg
splice _           NoSG sg   = sg
splice (rewire, s) sg_f sg_g = splice' (rewire, s) sg_f sg_g


splice' :: ((Circuit -> Circuit -> ([Edge], (Pins, Pins))), String) -> Circuit -> Circuit -> Circuit
splice' (rewire, s) sg_f sg_g 
    = MkSG { label   = (label sg_f') ++ s ++ (label sg_g')
           , compID  = 0
           , nodes   = sg_f': sg_g' : []
           , edges   = es
           , sinks   = srcs 
           , sources = snks
           }
    where sg_f'              = alterCompIDs 1                    sg_f
          sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
          (es, (srcs, snks)) = rewire sg_f' sg_g'


