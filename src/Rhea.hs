module Rhea
    ( run
    ) where

import qualified Rhea.Core.Application as A
import Rhea.Input.Input
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
import Linear
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.IORef
import Rhea.Input.Mouse (MouseInfo(frontVec))

type VEBObject =
  (VertexArray, Buffer, Buffer)

data Env = Env
  { window     :: Window
  , shader     :: Shader
  , vao        :: VertexArray
  , vbo        :: Buffer
  , texture    :: Texture
  , camera     :: Camera
  , time       :: A.Time
  }

cameraSpeed :: Float
cameraSpeed = 3.9

updateEnv'Time :: (Functor f) => (A.Time -> f A.Time) -> Env -> f Env
updateEnv'Time f env = (\v -> env {time = v}) <$> f (time env)

updateEnv'Window :: (Functor f) => (Window -> f Window) -> Env -> f Env
updateEnv'Window f env = (\v -> env {window = v}) <$> f (window env)

updateEnv'Camera'ByKey :: Env -> IO Env
updateEnv'Camera'ByKey env = (\v -> env {camera = v}) <$>
  updateCamera env (camera env)

updateEnv'Camera'ByMouse :: Env -> IO Env
updateEnv'Camera'ByMouse env = (\mi -> 
  env { camera = ( camera env ) { cameraFront = frontVec mi } }
  ) <$> readIORef (mouseInfo $ input $ window env)

updateCamera :: Env -> Camera -> IO Camera
updateCamera env =
  onPressed'Key (input $ window env) $
    \k c ->
      let speed = cameraSpeed * A.deltaTime (time env) in
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

toogleMouseInput :: Env -> IO Env
toogleMouseInput env = do
  onPressed'Key (input $ window env) 
    (\key e -> case key of
      GLFW.Key'A ->
        let _ = print "key" in e
      GLFW.Key'T  -> 
        let _ = print "key" in e
      _ -> 
        let _ = print key in e
    ) env

contextualEnv :: Maybe Env -> IO ()
contextualEnv Nothing = return ()
contextualEnv (Just env) = do
  window env |> closeCallback
  A.run
    env ( updateEnv'Time A.updateTime'Start
      >=> renderEnv
      >=> updateEnv'Camera'ByKey
      >=> toogleMouseInput
      >=> updateEnv'Camera'ByMouse
      >=> updateEnv'Window updateWindow
      >=> updateEnv'Time A.updateTime'End
      ) onFinish

genVertecies :: IO VEBObject
genVertecies = do

  vao_ <- makeVertexArray
  vbo_ <- makeVertexBuffer cubes

  linkBuffer vbo_ 0 3 (floatSize * 5) (floatSize * 0)
  linkBuffer vbo_ 2 2 (floatSize * 5) (floatSize * 3)

  return (vao_, vbo_, undefined)

renderEnv :: Env -> IO Env
renderEnv env@(Env window shader vao _ texture camera _) = do

  clear
  clearColor Charcoal

  let (VideoMode windowWidth windowHeight _) = videoMode window
  let model       = L.mkTransformation (L.axisAngle (V3 0.5 1 0) 1) (V3 0 0 0)
  let view        = cameraView camera
  let projection  = L.perspective 45.0 ( realToFrac windowWidth / realToFrac windowHeight ) 0.1 100.0

  useShader shader
  updateUniform shader |> forM_
    [ UniformMatrix4f "projection" projection
    , UniformMatrix4f "view" view
    , UniformMatrix4f "model" model
    , Uniform1i "uTexture" (fromIntegral $ textureSlot texture)
    ]

  bindTexture texture

  bindVao vao
  glDrawArrays GL_TRIANGLES 0 36

  unbindVao
  unbindTexture

  swap window
  viewport window

  return env

onFinish :: Env -> IO ()
onFinish env = do
  close $ shader env
  close $ vbo env
  deleteVao $ vao env
  destroy $ window env

mainEnv :: IO (Maybe Env)
mainEnv = do
  createWindow (VideoMode 1080 720 "Demo") >>= contextualizeWindow

contextualizeWindow :: Maybe Window -> IO (Maybe Env)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just window_) = do

  enable

  (vao_, vbo_, _) <- genVertecies

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
    vao_ vbo_
    texture_
    camera
    ( A.Time 0 0 0 )

run :: IO ()
run = do
  A.initApp
  mainEnv >>= contextualEnv

