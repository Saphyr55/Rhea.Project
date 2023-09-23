module Rhea.Core.Application 
  ( initApp
  , run
  , getTime
  ) where

import Control.Monad (forever)
import qualified Graphics.UI.GLFW as GLFW

type Handler c = c -> IO c

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

run :: c -> Handler c -> (c -> IO ()) -> IO ()
run context handler onFinish = do
  run'Loop context handler
  onFinish context
  GLFW.terminate

run'Loop :: c -> Handler c -> IO ()
run'Loop context handler = do
  ctx <- handler context
  forever $ do
    GLFW.pollEvents
    run'Loop ctx handler

getTime :: IO (Maybe Double)
getTime = GLFW.getTime
