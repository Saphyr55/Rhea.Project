module Core.Application (run) where

import Control.Monad (forever)
import qualified Graphics.UI.GLFW as GLFW

type Handler c = c -> IO c

initGLFW :: IO ()
initGLFW = do
  _ <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)
  
run :: c -> Handler c -> IO ()
run context handler = do
  initGLFW
  auxRunning context handler
  GLFW.terminate

auxRunning :: c -> Handler c -> IO ()
auxRunning context handler = do
  ctx <- handler context
  forever $ do
    auxRunning ctx handler
  