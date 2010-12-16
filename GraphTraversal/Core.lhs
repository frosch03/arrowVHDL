> module GraphTraversal.Core
> where

The fundamential datatype is a structured graph. 
Every node has:
  Name          := String
  [InPin]       := [Int]
  [OutPin]      := [Int]
  Component ID  := Int
  Format String := String

> data Core 
>   = MkSG { name    :: String
>          , inPins  :: [Int]
>          , outPins :: [Int]
>          , compId  :: Int
>          , fmtStrg :: String
>          }
            
