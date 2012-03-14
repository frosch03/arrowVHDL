module Grid.Splice
where

import Grid.Core
import Grid.Workers (alterCompIDs)
import Grid.Sensors (maxCompID)

splice :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
splice _           sg NoDescriptor = sg
splice _           NoDescriptor sg = sg
splice (rewire, s) sg_f sg_g       = splice' (rewire, s) sg_f sg_g


splice' :: ((CircuitDescriptor -> CircuitDescriptor -> ([Edge], (Pins, Pins))), String) -> CircuitDescriptor -> CircuitDescriptor -> CircuitDescriptor
splice' (rewire, s) sg_f sg_g 
    = MkDescriptor 
           { label   = (label sg_f') ++ s ++ (label sg_g')
           , compID  = 0
           , nodes   = sg_f': sg_g' : []
           , edges   = es
           , sinks   = srcs 
           , sources = snks
           }
    where sg_f'              = alterCompIDs 1                    sg_f
          sg_g'              = alterCompIDs (maxCompID sg_f' +1) sg_g
          (es, (srcs, snks)) = rewire sg_f' sg_g'


