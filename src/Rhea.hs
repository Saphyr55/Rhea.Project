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
import Graphics.OpenGL.VertexArray
import Core.Resources
import Graphics.Rendering.Shader
import Graphics.OpenGL.Shader
import GHC.TypeLits (Nat)
import Graphics.OpenGL.Buffer
import Core.Closeable (close)

data Context = Context
  { window :: Window,
    renderer :: GLRenderer,
    shader :: GLShader,
    vao :: VertexArrayObject,
    vbo :: Buffer ,
    ebo :: Buffer }

type VEBObject = 
  (VertexArrayObject, Buffer, Buffer)

verticies :: [Float]
verticies =
  [  0.5,  0.5, 0.0,  -- top right
     0.5, -0.5, 0.0,  -- bottom right
    -0.5, -0.5, 0.0,  -- bottom left
    -0.5,  0.5, 0.0   -- top left 
  ]

indices :: [Nat]
indices = 
  [ 0, 1, 3,   -- first triangle
    1, 2, 3    -- second triangle
  ]

defaultShader :: IO GLShader
defaultShader = do
  resVert <- readResource "/Shaders/Default.vert"
  resFrag <- readResource "/Shaders/Default.frag"
  makeShader 
    [ (VertexShader, resVert),
      (FragmentShader, resFrag) 
    ]

genVertecies :: IO VEBObject
genVertecies = do

  vao_ <- makeVao
  vbo_ <- makeVertexBuffer verticies
  ebo_ <- makeElementBuffer indices

  linkBuffer'offset vbo_ 0 3
    $ fromIntegral
    $ sizeOf (0.0 :: GLfloat) * 3

  return (vao_, vbo_, ebo_)

mainHandler :: Context -> IO Context
mainHandler context = do
  
  clear $ renderer context
  clearColor $= renderer context $ Charcoal

  useShader $ shader context
  bindVao $ vao context
  glDrawElements 
    GL_TRIANGLES 
    (fromIntegral $ length indices) 
    GL_UNSIGNED_INT 
    nullPtr
  unbindVao

  viewport $= renderer context $ window context

  swap $ window context

  newWindow <- updateWindow $ window context

  return $ Context 
    newWindow
    (renderer context)
    (shader context)
    (vao context)
    (vbo context)
    (ebo context)

mainContext :: IO (Maybe Context)
mainContext = do
  m <- makeWindow $ VideoMode 1080 720 "Demo"
  contextualizeWindow m

onFinish :: Context -> IO ()
onFinish context = do
  deleteShader $ shader context
  close $ vbo context
  close $ ebo context
  deleteVao $ vao context
  destroy $ window context

contextualizeWindow :: Maybe Window -> IO (Maybe Context)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just w) = do
  (vao_, vbo_, ebo_) <- genVertecies
  s <- defaultShader
  return $ Just $ Context w GLRenderer s vao_ vbo_ ebo_

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