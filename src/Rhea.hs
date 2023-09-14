module Rhea
    ( run
    ) where

import qualified Core.Application as App
import qualified Graphics.Window as Window
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW (swapBuffers)
import Foreign

data Context = Context
  { window :: Window.Window }

videoMode :: Window.VideoMode
videoMode =
  Window.VideoMode
    1080
    720
    "Demo"

verticies :: [Float]
verticies =
  [
    -0.5, -0.5, 0.0, -- first vertex
     0.5, -0.5, 0.0, -- second vertex
     0.0,  0.5, 0.0 -- third vertex
  ] :: [Float]

verticesSize :: Integer
verticesSize =
  fromIntegral $ sizeOf (0.0 :: Float) * length verticies

handler :: Context -> IO Context
handler context = do
  GL.clearColor $= Color4 0.1 0.2 0.2 1
  GL.clear [ColorBuffer]
  GLFW.swapBuffers $ Window.handler $ window context
  return context

mainContext :: IO (Maybe Context)
mainContext = do
  m <- Window.makeWindow videoMode
  setupWindow m

onFinish :: Context -> IO ()
onFinish context = do
  Window.destroy $ window context

setupWindow :: Maybe Window.Window -> IO (Maybe Context)
setupWindow Nothing = return Nothing
setupWindow (Just w) = return $ Just $ Context w

contextual :: Maybe Context -> IO ()
contextual Nothing = return ()
contextual (Just ctx) = do
  Window.closeCallback $ window ctx
  App.run ctx handler onFinish

run :: IO ()
run = do
  App.initApp
  maybeContext <- mainContext
  contextual maybeContext