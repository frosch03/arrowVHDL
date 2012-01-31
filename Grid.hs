module Grid where

import Grid.Core
import Grid.Auxillary
import Grid.Workers
import Grid.Tests


(...), new              :: Netlist

(.>.), connect_together :: Netlist -> Circuit -> Netlist
(.=.), frame_together   :: Circuit -> Circuit -> Netlist

(.^.), filter_out_of    :: Circuit -> Netlist -> Netlist


connect_together = connect
(.>.) = connect_together

frame_together = combine
(.=.) = frame_together

filter_out_of c = dropCircuit (hasLabel.label $ c)
(.^.) = filter_out_of
