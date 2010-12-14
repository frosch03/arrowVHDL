module Traversal.Structure
where

data Structure = Annotate { name         :: String
                          , returnvalue  :: Maybe Bool
                          , formatstring :: String 
                          , inputs       :: [String]
                          , output       :: String
                          , predecessor  :: Either [Structure] [Structure]
                          }
--               deriving (Show)

instance Show (Structure) where
    show (Annotate { name        = name'
                   , predecessor = predecessor'
                   } )      
                   = either normal boxed predecessor' 
        where normal p = if (null p) then name' else name' ++ "[" ++ parts p ++ "]"
              boxed  p = if (null p) then name' else name' ++ "(" ++ parts p ++ ")"
              parts  p = foldl1 (\x y -> show x ++ ", " ++ show y) $ map show p
