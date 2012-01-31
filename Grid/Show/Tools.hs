module Grid.Show.Tools
where


-- break is a function, that appends a newline character to the 
-- end of a string. 

break :: String -> String
break = flip (++) "\n"
