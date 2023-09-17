module Rhea.Graphics.Rendering.Renderer 
  ( viewport,
    clearColor,
    clear
  ) where

import Rhea.Graphics.Color (Color)
import Rhea.Graphics.Window (Window)
import qualified Rhea.Graphics.OpenGL.Renderer as GL

viewport :: Window -> IO ()
viewport = GL.viewport

clearColor :: Color -> IO ()
clearColor = GL.clearColor

clear :: IO ()
clear = GL.clear
