module Core.Application (initApp, run) where

import Control.Monad (forever)
import qualified Graphics.UI.GLFW as GLFW

type Handler c = c -> IO c

initApp :: IO ()
initApp = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)

run :: c -> Handler c -> (c -> IO ()) -> IO ()
run context handler onFinish = do
  auxRun context handler
  GLFW.terminate
  onFinish context

auxRun :: c -> Handler c -> IO ()
auxRun context handler = do
  ctx <- handler context
  forever $ do 
    GLFW.pollEvents
    auxRun ctx handler
  