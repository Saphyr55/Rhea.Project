module Rhea
    ( run
    ) where

import qualified Rhea.Core.Application as A
import Rhea.Core.Common
import Rhea.Graphics.Image
import Rhea.Graphics.Window
import Rhea.Graphics.Rendering.Renderer
import Rhea.Graphics.Color
import Rhea.Graphics.OpenGL.Buffer
import Rhea.Graphics.OpenGL.VertexArray
import Rhea.Graphics.Rendering.Texture
import Rhea.Graphics.Rendering.Shader
import Rhea.Graphics.Rendering.ShaderType
import Rhea.Graphics.Camera
import Graphics.GL
import qualified Linear as L
import Linear ( V3(..) )
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.Word
import Rhea.Input.Input

type VEBObject =
  (VertexArray, Buffer, Buffer)

data Env = Env
  { window     :: Window
  , shader     :: Shader
  , vao        :: VertexArray
  , vbo        :: Buffer
  , ebo        :: Buffer
  , texture    :: Texture
  , camera     :: Camera
  , time       :: A.Time
  }

verticies :: [Float]
verticies =
  [  0.5,  0.5, 0.0,  1.0, 0.0, 0.0,  1.0, 1.0,  -- top right
     0.5, -0.5, 0.0,  0.0, 1.0, 0.0,  1.0, 0.0,  -- bottom right
    -0.5, -0.5, 0.0,  0.0, 0.0, 1.0,  0.0, 0.0,  -- bottom left
    -0.5,  0.5, 0.0,  1.0, 1.0, 0.0,  0.0, 1.0   -- top left
  ]

indices :: [Word64]
indices =
  [ 0, 1, 3,   -- first triangle
    1, 2, 3    -- second triangle
  ]

contextualEnv :: Maybe Env -> IO ()
contextualEnv Nothing = return ()
contextualEnv (Just env) = do
  closeCallback $ window env
  A.run
    env ( updateTime'Start
      >=> render'Env
      >=> updateWindow'Env
      >=> updateCamera'Env
      >=> updateTime'End
      ) onFinish

genVertecies :: IO VEBObject
genVertecies = do

  vao_ <- makeVertexArray
  vbo_ <- makeVertexBuffer cubes
  -- ebo_ <- makeElementBuffer indices

  linkBuffer vbo_ 0 3 (floatSize * 5) (floatSize * 0)
  linkBuffer vbo_ 2 2 (floatSize * 5) (floatSize * 3)

  return (vao_, vbo_, undefined)

render'Env :: Env -> IO Env
render'Env env@(Env window shader vao vbo ebo texture camera time) = do

  clear
  clearColor Charcoal

  let uu'Partial = updateUniform shader

  let (VideoMode windowWidth windowHeight _) = videoMode window

  let model       = L.mkTransformation (L.axisAngle (V3 0.5 1 0) 1) (V3 0 0 0)
  let view        = cameraView camera
  let projection  = L.perspective 45.0 ( realToFrac windowWidth / realToFrac windowHeight ) 0.1 100.0

  useShader shader

  uu'Partial $ UniformMatrix4f "projection" projection
  uu'Partial $ UniformMatrix4f "view"       view
  uu'Partial $ UniformMatrix4f "model"      model
  uu'Partial $ Uniform1i "uTexture" (fromIntegral $ textureSlot texture)

  bindTexture texture

  bindVao vao

  glDrawArrays GL_TRIANGLES 0 36

  unbindVao

  unbindTexture

  swap window

  viewport window

  return env

updateTime'Start :: Env -> IO Env
updateTime'Start env = do
  t <- A.updateTime'Start $ time env
  return env {time = t}

updateTime'End :: Env -> IO Env
updateTime'End env = do
  t <- A.updateTime'End $ time env
  return env {time = t}

updateWindow'Env :: Env -> IO Env
updateWindow'Env env = do
  w <- updateWindow $ window env
  return env { window = w }

updateCamera'Env :: Env -> IO Env
updateCamera'Env env = do
  c <- updateCamera env (camera env)
  return env { camera = c }

updateCamera :: Env -> Camera -> IO Camera
updateCamera env =
  onPressed (input $ window env) $
    \k c ->
      let speed = cameraSpeed env 5 in
      case k of
        GLFW.Key'W -> c { cameraPos = cameraPos c + 
          ( cameraFront c L.^* speed ) }
        GLFW.Key'S -> c { cameraPos = cameraPos c - 
          ( cameraFront c L.^* speed ) }
        GLFW.Key'A -> c { cameraPos = cameraPos c -
          ( speed L.*^ L.cross (cameraFront c) (cameraUp c) |> L.normalize ) }
        GLFW.Key'D -> c { cameraPos = cameraPos c + 
          ( speed L.*^ L.cross (cameraFront c) (cameraUp c) |> L.normalize ) }
        _ -> c

cameraSpeed :: Env -> Float -> Float
cameraSpeed env speed = speed * A.deltaTime (time env)

onFinish :: Env -> IO ()
onFinish env = do
  close $ shader env
  close $ vbo env
  -- close $ ebo env
  deleteVao $ vao env
  destroy $ window env

mainEnv :: IO (Maybe Env)
mainEnv = do
  createWindow (VideoMode 1080 720 "Demo") >>= contextualizeWindow

contextualizeWindow :: Maybe Window -> IO (Maybe Env)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just window_) = do

  enable

  (vao_, vbo_, ebo_) <- genVertecies

  shader_ <- defaultShader

  image_ <- image "Textures/Wall.jpg"

  texture_ <- createTexture image_ 0

  let camera = Camera {
       cameraPos    = V3 0 0 3,
       cameraFront  = V3 0 0 (-1),
       cameraUp     = V3 0 1 0 }

  return $ Just $ Env
    window_
    shader_
    vao_ vbo_  ebo_
    texture_
    camera
    ( A.Time 0 0 0 )

run :: IO ()
run = do
  A.initApp
  mainEnv >>= contextualEnv

