module Graphics.Rendering.Renderer ( RenderSystem(..) ) where

import Graphics.Color (Color)
import Graphics.Window (Window)

class RenderSystem c where
  
  viewport :: c -> Window -> IO ()

  clearColor :: c -> Color -> IO ()
    
  clear :: c -> IO ()
  