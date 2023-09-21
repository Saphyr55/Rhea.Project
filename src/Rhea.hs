module Rhea
    ( run
    ) where

import qualified Rhea.Core.Application as A
import Rhea.Graphics.Window
import Rhea.Graphics.Rendering.Renderer
import Rhea.Graphics.Color
import Rhea.Core.Common
import Rhea.Core.Resources
import Rhea.Graphics.OpenGL.Buffer
import Rhea.Graphics.OpenGL.VertexArray
import Rhea.Graphics.Rendering.Shader
import Rhea.Graphics.Rendering.ShaderType
import Rhea.Math.Vector
import Graphics.GL
import GHC.TypeLits
import Foreign
import GHC.Float

data Env = Env
  { window :: Window,
    shader :: Shader,
    vao :: VertexArray,
    vbo :: Buffer ,
    ebo :: Buffer }

type VEBObject =
  (VertexArray, Buffer, Buffer)

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

defaultShader :: IO Shader
defaultShader = do
  resVert <- readResource "/Shaders/Default.vert"
  resFrag <- readResource "/Shaders/Default.frag"
  createShader
    [ (VertexShader, resVert),
      (FragmentShader, resFrag)
    ]

genVertecies :: IO VEBObject
genVertecies = do

  vao_ <- makeVertexArray
  vbo_ <- makeVertexBuffer verticies
  ebo_ <- makeElementBuffer indices
  
  linkBuffer vbo_ 0 3 (fromIntegral $ sizeOf (0.0 :: GLfloat) * 3) nullPtr

  return (vao_, vbo_, ebo_)

mainHandler :: Env -> IO Env
mainHandler context = do
  
  Just time <- A.getTime

  clear
  clearColor Charcoal

  updateUniform
    $= shader context
    $ Uniform3f "uColor" 
    $ Vector3 0.0 (double2Float $ 0.5 + (sin time / 2)) 0.0

  bindVao $ vao context
  glDrawElements
    GL_TRIANGLES
    (fromIntegral $ length indices)
    GL_UNSIGNED_INT
    nullPtr
  unbindVao

  swap $ window context

  viewport $ window context

  newWindow <- updateWindow $ window context

  return $ Env
    newWindow
    (shader context)
    (vao context)
    (vbo context)
    (ebo context)


mainEnv :: IO (Maybe Env)
mainEnv = do
  m <- makeWindow $ VideoMode 1080 720 "Demo"
  contextualizeWindow m

onFinish :: Env -> IO ()
onFinish context = do
  close $ shader context
  close $ vbo context
  close $ ebo context
  deleteVao $ vao context
  destroy $ window context

contextualizeWindow :: Maybe Window -> IO (Maybe Env)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just w) = do
  (vao_, vbo_, ebo_) <- genVertecies
  s <- defaultShader
  return $ Just $ Env w s vao_ vbo_ ebo_

contextualEnv :: Maybe Env -> IO ()
contextualEnv Nothing = return ()
contextualEnv (Just ctx) = do
  closeCallback $ window ctx
  A.run ctx mainHandler onFinish

run :: IO ()
run = do
  A.initApp
  maybeEnv <- mainEnv
  contextualEnv maybeEnv