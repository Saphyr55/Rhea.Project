{-# LANGUAGE InstanceSigs #-}
module Graphics.OpenGL.Renderer where

import Graphics.GL
import Graphics.OpenGL.OpenGL
import Graphics.Color
import Graphics.Rendering.Renderer ( RenderSystem(..) )

data GLRenderer = GLRenderer

instance RenderSystem GLRenderer where

  clearColor :: GLRenderer -> Color -> IO ()
  clearColor _ c =
    glClearColor
      (mapGlfloat $ red c) 
      (mapGlfloat $ green c)
      (mapGlfloat $ blue c) 
      (mapGlfloat $ alpha c)

  clear :: GLRenderer -> IO ()
  clear _ = 
    glClear GL_COLOR_BUFFER_BIT
    

