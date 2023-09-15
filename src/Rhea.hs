module Rhea
    ( run
    ) where

import qualified Core.Application as Application
import Graphics.Window
import Graphics.Color (Color (..))
import Graphics.Rendering.Renderer ( RenderSystem (..) )
import Graphics.OpenGL.Renderer as Renderer ( Renderer (..) )

data Context = Context
  { window   :: Window,
    renderer :: Renderer.Renderer }

videoMode :: VideoMode
videoMode = VideoMode 1080 720 "Demo"

mainHandler :: Context -> IO Context
mainHandler context = do
  clearColor (renderer context) Charcoal
  clear $ renderer context
  swap $ window context
  return context

mainContext :: IO (Maybe Context)
mainContext = do
  m <- makeWindow videoMode
  setupWindow m

onFinish :: Context -> IO ()
onFinish context = do
  destroy $ window context

setupWindow :: Maybe Window -> IO (Maybe Context)
setupWindow Nothing = return Nothing
setupWindow (Just w) = do
  return $ Just $ Context w GLRenderer

contextual :: Maybe Context -> IO ()
contextual Nothing = return ()
contextual (Just ctx) = do
  closeCallback $ window ctx
  Application.run ctx mainHandler onFinish

run :: IO ()
run = do
  Application.initApp
  maybeContext <- mainContext
  contextual maybeContext