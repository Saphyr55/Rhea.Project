module Rhea
    ( run
    ) where

import qualified Rhea.Core.Application as A

import Rhea.Graphics.Image
import Rhea.Graphics.Window
import Rhea.Graphics.Rendering.Renderer
import Rhea.Graphics.Color
import Rhea.Core.Common
import Rhea.Core.Resources
import Rhea.Graphics.OpenGL.Buffer
import Rhea.Graphics.OpenGL.VertexArray
import Rhea.Graphics.OpenGL.Texture
import Rhea.Graphics.Rendering.Shader
import Rhea.Graphics.Rendering.ShaderType
import Graphics.GL
import Foreign

type VEBObject =
  (VertexArray, Buffer, Buffer)

data Env = Env
  { window  :: Window
  , shader  :: Shader
  , vao     :: VertexArray
  , vbo     :: Buffer
  , ebo     :: Buffer
  , texture :: Word32
  }

verticies :: [Float]
verticies =
  [  0.5,  0.5, 0.0,  0.0, 0.0, 0.0, 0.0,  1.0, 1.0,  -- top right
     0.5, -0.5, 0.0,  0.0, 0.0, 0.0, 0.0,  1.0, 0.0,  -- bottom right
    -0.5, -0.5, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0,  -- bottom left
    -0.5,  0.5, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 1.0   -- top left
  ]

indices :: [Word64]
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

  linkBuffer vbo_ 0 3 (fromIntegral $ sizeOf (0 :: Float) * 9) 
  linkBuffer vbo_ 1 4 (fromIntegral $ sizeOf (0 :: Float) * 9)
  linkBuffer vbo_ 2 2 (fromIntegral $ sizeOf (0 :: Float) * 9)

  return (vao_, vbo_, ebo_)

mainHandler :: Env -> IO Env
mainHandler context = do

  clear
  clearColor Charcoal
  handleError

  useShader $ shader context

  updateUniform $= shader context $ Uniform1i "uTexture" 0

  bindTexture $ texture context

  bindVao $ vao context

  glDrawElements
      GL_TRIANGLES
      (fromIntegral $ length indices)
      GL_UNSIGNED_INT
      nullPtr
      
  unbindVao

  unbindTexture

  swap $ window context

  viewport $ window context

  newWindow <- updateWindow $ window context

  return $ Env
    newWindow
    (shader context)
    (vao context)
    (vbo context)
    (ebo context)
    (texture context)

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
  i <- image "Textures/Wall.jpg"
  tex <- makeTexture i
  return $ Just $ Env w s vao_ vbo_ ebo_ tex

contextualEnv :: Maybe Env -> IO ()
contextualEnv Nothing = return ()
contextualEnv (Just ctx) = do
  closeCallback $ window ctx
  A.run ctx mainHandler onFinish

handleError :: IO ()
handleError = do 
  e <- glGetError 
  case e of
    GL_NO_ERROR -> return ()
    _ -> putStrLn "Error with opengl."

run :: IO ()
run = do
  A.initApp
  maybeEnv <- mainEnv
  contextualEnv maybeEnv