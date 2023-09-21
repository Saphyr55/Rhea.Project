module Rhea.Core.Resources (readResource) where
import Paths_RheaProject

readResource :: String -> IO String
readResource path = do
    file <- getDataFileName ("res/" ++ path)
    readFile file
