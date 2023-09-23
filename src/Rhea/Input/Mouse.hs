module Rhea.Input.Mouse 
  ( MouseInfo(..) 
  , cursorPosCallback
  ) where

import Linear
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Fixed

data MouseInfo = MouseInfo
  { lastXY :: Maybe (Double, Double)
  , oldPitchYaw :: (Double, Double) 
  , frontVec :: V3 Float
  } deriving ( Show )

cursorPosCallback :: IORef MouseInfo -> GLFW.CursorPosCallback
cursorPosCallback ref _ xpos ypos = do
  -- putStrLn $ "x: " ++ show x ++ ", y:" ++ show y
  modifyIORef ref $ \oldInfo ->
    let (lastX, lastY) = case lastXY oldInfo of
          Nothing -> (xpos, ypos)
          (Just (lastX, lastY)) -> (lastX, lastY)
        sensitivity = 0.05
        xoffset = (xpos - lastX) * sensitivity
        yoffset = (lastY - ypos) * sensitivity
        lastX' = xpos
        lastY' = ypos
        (oldPitch, oldYaw) = oldPitchYaw oldInfo
        newYaw = (oldYaw + xoffset) `mod'` 360.0
        newPitch = min (max (oldPitch + yoffset) (-89)) 89
        toRadians = realToFrac . (* (pi / 180)) :: Double -> Float
        pitchR = toRadians newPitch
        yawR = toRadians newYaw
        front = normalize $ V3 (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)
     in MouseInfo (Just (lastX', lastY')) (newPitch, newYaw) front