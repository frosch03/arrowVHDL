module Test where


infix 9 &
infix 1 #

type NetList = ( [Source]
               , [Sink]
               , [Component]
               , [Wire]
               )



data Bit = Bot | Low | High | Top
    deriving Eq


type Wire = (Source, Sink)


data Source 
    = Constant Bit
    | Global   String
    | Input    String
    | OutPort  Component Pin
    deriving Eq

type Pin  = Int


data Sink
    = Output String
    | InPort Component Pin Source
    deriving Eq


data Component 
    = Comp { compKind   :: CompKind
           , compName   :: CompName
           , compInputs :: CompInputs
           }
    deriving Eq

type CompKind   = String
type CompName   = [CompNamePiece]
type CompInputs = [Sink]

baseCompName :: CompName
baseCompName  = []

data CompNamePiece 
    = BlockName String
    | EquName   Int
    | NodeName  Int
    deriving Eq


isInput :: Source -> Bool
isInput (Input _) = True
isInput otherwise = False

isOutput :: Sink -> Bool
isOutput (Output _) = True
isOutput otherwise  = False

inWires :: Component -> [Wire]
inWires = map f . compInputs
    where f :: Sink -> Wire
          f snk@(InPort _ _ src) = (src, snk)

follow 
    :: [Source]
    -> [Sink]
    -> [Wire]
    -> [Component]
    -> [Wire]
    -> NetList
follow is os (h@(src, snk):hs) cs ws 
    = case src of
        Constant x  -> follow is' os' hs cs ws'
        Input x     -> follow is' os' hs cs ws'
        OutPort c p -> if c `elem` cs
                        then follow is' os' hs cs ws'
                        else follow is' os' (inWires c ++ hs) (c:cs) ws'
    where is' = addSource src is
          os' = addSink snk os
          ws' = h:ws

addSource :: Source -> [Source] -> [Source]
addSource src is = 
    if isInput src && src `notElem` is
        then src:is
        else is

addSink :: Sink -> [Sink] -> [Sink]
addSink snk os = 
    if isOutput snk && snk `notElem` os
        then snk:os
        else os



block :: String -> CompName -> CompName
block s cn = BlockName s : cn

newEquName :: Int -> CompName -> CompName
newEquName i cn = NodeName 0 : EquName i : cn

newNodeName :: CompName -> Int -> CompName
newNodeName (NodeName x : xs) i = NodeName (x + i) : xs



type Signal = CompName -> Source

mkInput :: String  -> Signal
mkInput s = (\_ -> Input s)

mkOutput :: Signal -> Source
mkOutput f = f baseCompName

forcename :: CompName -> Signal -> Signal
forcename cn f = (\_ -> f cn)

(#) = forcename

(&) :: Int -> CompName -> CompName
(&) = newEquName

-- Show instances
showBit :: Bit -> String
showBit Bot  = "B"
showBit Low  = "0"
showBit High = "1"
showBit Top  = "T"

showWire :: (Component -> String) -> Wire -> String
showWire f (src, snk) = showSource f src ++ "  -->  " ++ showSink f snk

showSource :: (Component -> String) -> Source -> String
showSource _ (Constant x)  = showBit x
showSource _ (Input x)     = "Input " ++ x
showSource f (OutPort x p) = f x ++ showPin p

showSink :: (Component -> String) -> Sink -> String
showSink _ (Output x)     = "Output " ++ x
showSink f (InPort c p _) = f c ++ showPin p

showPin :: Pin -> String
showPin p = '_' : show p

showCompNamePiece :: CompNamePiece -> String
showCompNamePiece (EquName n)   = ' ' : show n
showCompNamePiece (NodeName n)  = '.' : show n
showCompNamePiece (BlockName n) = ' ' : n

showCompName :: CompName -> String
showCompName = concat . map showCompNamePiece . reverse

showCompConcise :: [Component] -> Component -> String
showCompConcise cs c = show (compNo cs c)

compNo :: [Component] -> Component -> Int
compNo [] c     = -1
compNo (c:cs) x = if c == x then 1 else 1 + compNo cs x

showComp :: Component -> String
showComp (Comp k n xs) 
    = k ++ take (w - length k) (repeat ' ') ++ showCompName n
    where w = 10 :: Int

showNetList :: NetList -> String
showNetList (is, os, cs, ws) 
    = let label xs s  = "\n" ++ s ++ " (" ++ show (length xs) ++ ")\n"
          format f xs = concat [g f x i | (x, i) <- zip xs [1::Int ..]]
          g f x i     = fmtInt 5 i ++ ". " ++ f x ++ "\n"
      in  "\n" ++ take 30 (repeat '-')
      ++  "\nNetlist\n"
      ++  label is "Inputs"
      ++  format (showSource showComp) is
      ++  label os "Outputs"
      ++  format (showSink showComp) os
      ++  label cs "Components"
      ++  format showComp cs
      ++  label ws "Wires"
      ++  format (showWire (showCompConcise cs)) ws

fmtInt :: Int -> Int -> String
fmtInt w n = take (w - length xs) (repeat ' ') ++ xs
    where xs = show n
