This Module defines an Algebraic Data Type that represents a Transit-Structure for 
Pins. The Structure holds a list of components with their in- and out pins and the 
naming of those pins

> module GraphTraversal.PinTransit 
> where 

> import GraphTraversal.Core


+---------------------------------------------------------+
|                                                         |
|    [(CompID, ([(PinID, String)], [(PinID, String)]))]   |
|                                                         |
+---------------------------------------------------------+


First of all it is defined, what a named pin looks like

> type NamedPin = (PinID, String)
> type InNames  = [NamedPin]
> type OutNames = [NamedPin]


Then the component is connected with the named pins

> type NamedComp = (CompID, (InNames, OutNames))


Recall that usually there are more then one component in a Graph :-) which is 
represented as a list of NamedComps, although these list is not explicitly named.


Functions that work over the named component list type are:

* generateNamedComps
  A function that takes a Structured Graph 
  and produces a named component list

* getInPinNames
  A function which takes a named component list and a component id 
  and produces a list of pinID / name pairs

* getOutPinNames
  A function that takes a named component list and also a component id 
  and produces a list fo pinID / name pairs

* getInPinName 
  A function which takes a named component list, a component id and a pin id
  and produces a pinID / name pair

* getOutPinName
  A function that takes a named component list, a component id and a pin id
  and produces a pinID / name pair


Needed:
[ ] StructGraph data type (compID, nodes, sinks, sources)
[ ] Names (nameExI, nameExO, nameInI, nameInO)
[ ] namePins
[ ] 


> generateNamedComps :: StructGraph -> [NamedComp]
> generateNamedComps g = generateSuperNames g : (map generateSubNames $ nodes g)
>     where generateSuperNames g = ( compID g, ( namePins sinks   nameExI g
>                                              , namePins sources nameExO g
>                                              )
>                                  )
>           generateSubNames g   = ( compID g, ( namePins sinks   nameInI g
>                                              , namePins sources nameInO g
>                                              )
>                                  )


TODO: is fst the right function to get the in-names ??? 

> getNamedInPins :: [NamedComp] -> CompID -> InNames
> getNamedInPins = getPinNames fst

> getInPinNames :: [NamedComp] -> CompID -> [String]
> getInPinNames cname cid = map snd $ getNamedInPins cname cid



TODO: is snd the right function to get the in-names ??? 

> getNamedOutPins :: [NamedComp] -> CompID -> OutNames
> getNamedOutPins = getPinNames snd

> getOutPinNames :: [NamedComp] -> CompID -> [String]
> getOutPinNames cname cid = map snd $ getNamedOutPins cname cid

> getPinNames :: (([NamedPin], [NamedPin]) -> [NamedPin]) -> [NamedComp] -> CompID -> [(PinID, String)]
> getPinNames f cname cid
>     = concat
>     $ map f
>     $ map snd 
>     $ filter (\(x, _) -> x == cid)
>     $ cname


> getNamedInPin :: [NamedComp] -> CompID -> PinID -> NamedPin
> getNamedInPin = getPinName getNamedInPins

> getNamedOutPin :: [NamedComp] -> CompID -> PinID -> NamedPin
> getNamedOutPin = getPinName getNamedOutPins


> getInPinName :: [NamedComp] -> CompID -> PinID -> String
> getInPinName cname cid pid = snd $ getNamedInPin cname cid pid

> getOutPinName :: [NamedComp] -> CompID -> PinID -> String
> getOutPinName cname cid pid = snd $ getNamedOutPin cname cid pid


> getPinName :: ([NamedComp] -> CompID -> [NamedPin]) -> [NamedComp] -> CompID -> PinID -> NamedPin
> getPinName f cname cid pid 
>     = head 
>     $ filter (\(x, _) -> x == pid)
>     $ f cname cid


The namePins function takes a function that extracts a list of PinIDs out of an StructGraph.
(This could be the sinks or the sources functions) 
It also takes a StructGraph (suprise :)) and a String, that is prepended to the actual PinName.
This functions returns a list, where every element is a tuple of the actual named pin (a string)
and a part, that identifies the name.

> namePins :: (StructGraph -> Pins) -> String -> StructGraph -> [NamedPin]
> namePins f pre g
>     = map (\x -> (x, pre ++ (show x))) $ f g 
