module Rhea.Input.Input 
  ( Input(..) 
  ) where

import Data.IORef
import Data.Set
import qualified Graphics.UI.GLFW as GLFW
import qualified Rhea.Input.Mouse as M

data Input = Input 
  { keys   :: IORef (Set GLFW.Key)
  , mouses :: IORef M.MouseInfo
  }
