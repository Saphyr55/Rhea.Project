module Rhea.Input.Mouse 
  ( MouseInfo(..) 
  ) where

import Linear

data MouseInfo = MouseInfo
  { lastXY :: Maybe (Double, Double)
  , oldPitchYaw :: (Double, Double) 
  , frontVec :: V3 Float
  } deriving ( Show )

