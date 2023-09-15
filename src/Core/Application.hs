module Core.Application (initApp, run) where

import Control.Monad (forever)
import qualified Graphics.UI.GLFW as GLFW

type Handler c = c -> IO c

hints :: [GLFW.WindowHint]
hints =
  [
    GLFW.WindowHint'ContextVersionMajor 4,
    GLFW.WindowHint'ContextVersionMinor 5,
    GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
    GLFW.WindowHint'Resizable True
  ]

initApp :: IO ()
initApp = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  let _ = map GLFW.windowHint hints
  GLFW.windowHint $ GLFW.WindowHint'Resizable True

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
