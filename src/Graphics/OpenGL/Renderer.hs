{-# LANGUAGE InstanceSigs #-}
module Graphics.OpenGL.Renderer where

import Graphics.GL
import Graphics.Color ( alpha, blue, green, red, Color )
import Graphics.Rendering.Renderer ( RenderSystem(..) )
import Data.Bits
import Graphics.Window (Window(..), VideoMode (..))

data GLRenderer = GLRenderer

instance RenderSystem GLRenderer where

  viewport :: GLRenderer -> Window -> IO ()
  viewport _ window =  
    glViewport 0 0
      (fromIntegral $ width $ videoMode window)
      (fromIntegral $ height $ videoMode window)

  clearColor :: GLRenderer -> Color -> IO ()
  clearColor _ c =
    glClearColor
      (red c   :: GLfloat)
      (green c :: GLfloat)
      (blue c  :: GLfloat)
      (alpha c :: GLfloat)

  clear :: GLRenderer -> IO ()
  clear _ = 
    glClear $ 
      GL_COLOR_BUFFER_BIT .|.
      GL_DEPTH_BUFFER_BIT 
    

