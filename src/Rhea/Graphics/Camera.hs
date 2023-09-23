module Rhea.Graphics.Camera
 ( Camera(..)
 , cameraView
 , updateCamera
 ) where

import Linear
import Data.Set
import qualified Data.Set as S
import qualified Graphics.UI.GLFW as GLFW

data Camera = Camera 
  { cameraPos    :: V3 Float
  , cameraFront  :: V3 Float
  , cameraUp     :: V3 Float
  } deriving ( Show )

cameraView :: Camera -> M44 Float
cameraView (Camera pos front up) = 
  lookAt pos (pos + front) up

updateCamera :: Set GLFW.Key -> Float -> Camera -> Camera
updateCamera keySet speed cam@(Camera pos front up) =
  let moveVector =
        S.foldr (\key vec -> case key of
            GLFW.Key'W -> vec ^+^ front
            GLFW.Key'S -> vec ^-^ front
            GLFW.Key'A -> vec ^-^ normalize (cross front up)
            GLFW.Key'D -> vec ^+^ normalize (cross front up)
            _ -> vec)
          (V3 0 0 0)
          keySet
   in cam {cameraPos = pos ^+^ (speed *^ normalize moveVector)}
