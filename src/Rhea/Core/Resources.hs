module Rhea.Core.Resources 
  ( readResource
  , resourceFilepath
  ) where
import Paths_RheaProject

readResource :: String -> IO String
readResource path = do
    file <- resourceFilepath path
    readFile file

resourceFilepath :: String -> IO FilePath
resourceFilepath path = getDataFileName ("res/" ++ path)
