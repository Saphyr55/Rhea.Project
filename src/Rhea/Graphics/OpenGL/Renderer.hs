module Rhea.Graphics.OpenGL.Renderer 
  (
    viewport,
    clearColor,
    clear
  ) where

import Graphics.GL
import Rhea.Graphics.Color ( alpha, blue, green, red, Color )
import Data.Bits
import Rhea.Graphics.Window (VideoMode (..), Window (..))

viewport :: Window -> IO ()
viewport window =  
  glViewport 0 0
    (fromIntegral $ width $ videoMode window)
    (fromIntegral $ height $ videoMode window)

clearColor ::  Color -> IO ()
clearColor c =
  glClearColor
    (red c   :: GLfloat)
    (green c :: GLfloat)
    (blue c  :: GLfloat)
    (alpha c :: GLfloat)

clear :: IO ()
clear = 
  glClear $ 
    GL_COLOR_BUFFER_BIT .|.
    GL_DEPTH_BUFFER_BIT 
    

