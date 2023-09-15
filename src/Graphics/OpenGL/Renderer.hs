{-# LANGUAGE InstanceSigs #-}
module Graphics.OpenGL.Renderer where

import Graphics.Rendering.Renderer ( RenderSystem(..) )
import Graphics.Color ( Color, alpha, red, green, blue )
import qualified Graphics.Rendering.OpenGL as GL

data Renderer = GLRenderer 

instance RenderSystem Renderer where

  clearColor :: Renderer -> Color -> IO ()
  clearColor _ c = do
    GL.clearColor GL.$= 
      GL.Color4 
        ( red   c )
        ( green c )
        ( blue  c )
        ( alpha c )

  clear :: Renderer -> IO ()
  clear _ = do
    GL.clear [GL.ColorBuffer]