module Rhea.Graphics.OpenGL.Renderer 
  ( enable
  , viewport
  , clearColor
  , clear
  ) where
    
import Rhea.Graphics.Color ( Color, alpha, blue, green, red)
import Rhea.Graphics.Window ( VideoMode (..), Window (..) )
import Graphics.GL
import Data.Bits

viewport :: Window -> IO ()
viewport window =  
    glViewport 0 0
        (fromIntegral $ width $ videoMode window)
        (fromIntegral $ height $ videoMode window)

enable :: IO ()
enable = 
  glEnable GL_DEPTH_TEST

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
