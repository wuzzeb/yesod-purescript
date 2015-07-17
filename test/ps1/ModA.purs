module ModA where

abc :: String -> String
abc x = append x "Abc"

foreign import def :: String -> String
foreign import append :: String -> String -> String
