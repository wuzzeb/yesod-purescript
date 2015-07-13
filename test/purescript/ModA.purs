module A where

import Data.Maybe

data Hello = Hello { a :: Maybe String }

abc :: String -> String
abc x = x ++ "Abc"
