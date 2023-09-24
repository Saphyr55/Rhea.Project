module Rhea.Input.Mouse 
  ( MouseInfo(..) 
  , mouseCallback
  ) where

import Linear
import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Data.Fixed

data MouseInfo = MouseInfo
  { lastXY      :: Maybe (Double, Double)
  , oldPitchYaw :: (Double, Double) 
  , frontVec    :: V3 Float
  } deriving ( Show )

mouseCallback :: IORef MouseInfo -> GLFW.CursorPosCallback
mouseCallback ref _ xpos ypos = do

  modifyIORef ref $ \oldInfo -> let

    (lastX, lastY) = 
      case lastXY oldInfo of
        Nothing  -> (xpos,ypos)
        (Just p) -> p

    sensitivity = 0.05

    xoffset = 
      (xpos - lastX) * sensitivity
    yoffset = 
      (lastY - ypos) * sensitivity

    lastX' = xpos
    lastY' = ypos

    (oldPitch, oldYaw) = oldPitchYaw oldInfo
    newYaw = (oldYaw + xoffset) `mod'` 360.0
    newPitch = max (oldPitch + yoffset) . min (-89) $ 89
    toRadians = realToFrac . (* (pi / 180) ) :: Double -> Float
    pitchR = toRadians newPitch
    yawR = toRadians newYaw

    front = 
      normalize $ V3 
        (cos yawR * cos pitchR) 
        (sin pitchR) 
        (sin yawR * cos pitchR)

    in MouseInfo 
      (Just (lastX',lastY')) 
      (newPitch, newYaw)
      front
