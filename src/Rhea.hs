module Rhea
    ( run
    ) where

import qualified Core.Application as A
import Graphics.Window
import Graphics.Rendering.Renderer
import Graphics.OpenGL.Renderer
import Graphics.Color
import Core.Common
import Foreign
import Graphics.GL
import Graphics.OpenGL.VertexBuffer (makeVbo, linkVbo, deleteVbo, VertexBufferObject)
import Graphics.OpenGL.VertexArray
import Core.Resources
import Graphics.Rendering.Shader
import Graphics.OpenGL.Shader

data Context = Context
  { window :: Window,
    renderer :: GLRenderer,
    shader :: GLShader,
    vao :: VertexArrayObject,
    vbo :: VertexBufferObject }

verticies :: [Float]
verticies =
  [ -0.5, -0.5, 0.0, -- first vertex
     0.5, -0.5, 0.0, -- second vertex
     0.0,  0.5, 0.0  -- third vertex
  ]

defaultShader :: IO GLShader
defaultShader = do
  resVert <- readResource "/Shaders/Default.vert"
  resFrag <- readResource "/Shaders/Default.frag"
  makeShader 
    [ (VertexShader, resVert),
      (FragmentShader, resFrag) 
    ]

genVertecies :: IO (VertexArrayObject, VertexBufferObject)
genVertecies = do
  vbo_ <- makeVbo verticies
  vao_ <- makeVao
  linkVbo vbo_ 0 3
    $ fromIntegral
    $ sizeOf (0.0 :: GLfloat) * 3
  return (vao_, vbo_)

mainVideoMode :: VideoMode
mainVideoMode =
  VideoMode 1080 720 "Demo"

mainHandler :: Context -> IO Context
mainHandler context = do

  clearColor $= renderer context $ Charcoal
  clear $ renderer context

  bindVao $ vao context
  glDrawArrays GL_TRIANGLES 0 3
  unbindVao

  viewport
    $= renderer context
    $ window context

  swap $ window context

  newWindow <- updateWindow $ window context

  return $ Context
    newWindow
    (renderer context)
    (shader context)
    (vao context)
    (vbo context)

mainContext :: IO (Maybe Context)
mainContext = do
  m <- makeWindow mainVideoMode
  contextualizeWindow m

onFinish :: Context -> IO ()
onFinish context = do
  deleteVbo $ vbo context
  deleteVao $ vao context
  destroy $ window context

contextualizeWindow :: Maybe Window -> IO (Maybe Context)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just w) = do
  (vao_, vbo_) <- genVertecies
  s <- defaultShader 
  return $ Just $ Context w GLRenderer s vao_ vbo_

contextual :: Maybe Context -> IO ()
contextual Nothing = return ()
contextual (Just ctx) = do
  closeCallback $ window ctx
  A.run ctx mainHandler onFinish

run :: IO ()
run = do
  A.initApp
  maybeEnv <- mainContext
  contextual maybeEnv