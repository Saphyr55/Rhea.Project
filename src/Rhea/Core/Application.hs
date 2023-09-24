module Rhea.Core.Application 
  ( Time (..)
  , initApp
  , run
  , getTime
  , updateTime'Start
  , updateTime'End
  ) where

import Control.Monad (forever)
import qualified Graphics.UI.GLFW as GLFW

data Time = Time 
  { deltaTime :: Float
  , lastTime  :: Float
  , valueTime :: Float
  }

hints :: [GLFW.WindowHint]
hints =
  [ GLFW.WindowHint'ContextVersionMajor 4,
    GLFW.WindowHint'ContextVersionMinor 5,
    GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
    GLFW.WindowHint'Resizable True ]

initApp :: IO ()
initApp = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  mapM_ GLFW.windowHint hints
  GLFW.windowHint $ GLFW.WindowHint'Resizable True

run :: c -> (c -> IO c) -> (c -> IO ()) -> IO ()
run context handler onFinish = do
  run'Forever context handler
  onFinish context
  GLFW.terminate

run'Forever :: c -> (c -> IO c) -> IO ()
run'Forever context handler = do
  ctx <- handler context
  forever $ do
    GLFW.pollEvents
    run'Forever ctx handler

getTime :: IO (Maybe Double)
getTime = GLFW.getTime

updateTime'Start :: Time -> IO Time
updateTime'Start t = do
  vl <- maybe 0 realToFrac <$> getTime
  return t { valueTime = vl }

updateTime'End :: Time -> IO Time
updateTime'End t = do
  return t
    { deltaTime = valueTime t - lastTime t
    , lastTime = valueTime t
    }
