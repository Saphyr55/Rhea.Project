module Graphics.Rendering.Renderer where

import Graphics.Color ( Color ) 

class RenderSystem c where

    clearColor :: c -> Color -> IO ()
    
    clear :: c -> IO ()
