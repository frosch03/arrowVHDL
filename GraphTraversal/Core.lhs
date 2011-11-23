> module GraphTraversal.Core
> where


First of all, lets name some basic types that are used through the whole
source...
There are the pins of a hardware component, and they are identified by an 
integer. The same holds for the identification of a whole component, which 
is also done by an integer. And than there is also a name for the list of
pins that a component holds (either the sink pins or the source pins).

> type PinID  = Int
> type CompID = Int
> type Pins   = [PinID]


An edge is like a wire between two pins on different components. So it is
identified by the component id and the pin id. This tupel is called an
Anchor, and for documentation reasons there are two different versions,
the SinkAnchor and the SourceAnchor.

> type Anchor       = (Maybe CompID, PinID)
> type SinkAnchor   = Anchor
> type SourceAnchor = Anchor


To translate the graph structure into a VHDL-sourcecode the anchors and the
edges are named and stored in lookup tables. There are two of them, one for 
the named pins and one for the named edges. For documentation there is also
a input output tuple, that holds the lookup table for the sinks and the 
sources.

> type NamedPins = [(String, Anchor)]
> type NamedSigs = [(String, Edge)]
> type NamedSnks = NamedPins
> type NamedSrcs = NamedPins
> type NamedIOs  = (NamedSnks, NamedSrcs)

> nameSig = "i"
> nameExI = "inc"
> nameExO = "out"
> nameInI = "e"
> nameInO = "a"


The structured graph is the fundamental datatype. It holds information
about its nodes and is identified by an unique id. Additionally the in- 
and out pins are listed as well as connections (edges) between the nodes.
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int

Also a name is needed later on to identify the components.
  Name          := String

It turns out, that a Node actually could hold another StructGraph, that 
defines sub structures. It also could hold no sub structure and has only 
a name and a component id.
Another constructor to be not forgotten is the empty graph, marked here
with NoSG. It's uses are i.e. to start the computation with runTraversal
where an empty graph has to be passed over to the computing arrow

> data Circuit
>   = MkSG { name    :: String
>          , compID  :: CompID
>          , nodes   :: [Circuit]
>          , edges   :: [Edge]
>          , sinks   :: Pins
>          , sources :: Pins
>          }
>   | NoSG
>
>   deriving (Eq)

Remember, a Sink is something that takes something  (INPUT)
and a Source is something that produces something (OUTPUT)

So the next datatype to be defined is an edge. The edge knows 
the pin/component it comes from, as well as the pin/component
it goes to.

> data Edge
>   = MkEdge { sourceInfo :: SourceAnchor
>            , sinkInfo   :: SinkAnchor
>            }
>   deriving (Eq)

   deriving (Eq, Show)
