module Rhea.Graphics.Camera
 ( Camera(..)
 , cameraView
 ) where

import Linear

data Camera = Camera 
  { cameraPos    :: V3 Float
  , cameraFront  :: V3 Float
  , cameraUp     :: V3 Float
  } deriving ( Show )

cameraView :: Camera -> M44 Float
cameraView (Camera pos front up) = 
  lookAt pos (pos + front) up

  
