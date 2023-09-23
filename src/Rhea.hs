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
import Rhea.Graphics.Rendering.Texture
import Rhea.Graphics.Rendering.Shader
import Rhea.Graphics.Rendering.ShaderType
import Graphics.GL
import Foreign
import qualified Linear as L
import Linear ( V3(..) )
import Rhea.Graphics.Camera
import Data.IORef
import qualified Rhea.Input.Input as I
import Rhea.Input.Mouse (MouseInfo(frontVec))
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad

type VEBObject =
  (VertexArray, Buffer, Buffer)

data Env = Env
  { cWindow   :: Window
  , cShader   :: Shader
  , vao       :: VertexArray
  , vbo       :: Buffer
  , ebo       :: Buffer
  , texture   :: Texture
  , envCamera :: Camera
  , deltaTime :: Float
  , lastTime  :: Float
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

  let floatSize = ( fromIntegral $ sizeOf (0 :: Float) ) :: Word32

  vao_ <- makeVertexArray
  vbo_ <- makeVertexBuffer cubes
  -- ebo_ <- makeElementBuffer indices

  linkBuffer vbo_ 0 3 (floatSize * 5) (floatSize * 0)
  linkBuffer vbo_ 2 2 (floatSize * 5) (floatSize * 3)

  return (vao_, vbo_, undefined)

mainHandler :: Env -> IO Env
mainHandler env = do

  valueTime <- maybe 0 realToFrac <$> A.getTime

  clear
  clearColor Charcoal
  handleError

  let shader = cShader env
  let window = cWindow env

  let uu'Partial = updateUniform shader

  let (VideoMode windowWidth windowHeight _) = videoMode $ cWindow env

  let model       = L.mkTransformation (L.axisAngle (V3 0.5 1 0) 1) (V3 0 0 0)
  let view        = cameraView $ envCamera env
  let projection  = L.perspective 45.0 ( realToFrac windowWidth / realToFrac windowHeight ) 0.1 100.0

  useShader shader

  uu'Partial $ UniformMatrix4f "projection" projection
  uu'Partial $ UniformMatrix4f "view"       view
  uu'Partial $ UniformMatrix4f "model"      model
  uu'Partial $ Uniform1i       "uTexture"   0

  bindTexture $ texture env

  bindVao $ vao env

  glDrawArrays GL_TRIANGLES 0 36

  unbindVao

  unbindTexture

  swap window

  viewport window

  newCamera <- updateCamera'IO env
  newWindow <- updateWindow window

  return $ Env
    newWindow
    (cShader env)
    (vao env)
    (vbo env)
    (ebo env)
    (texture env)
    newCamera
    (valueTime - lastTime env)
    valueTime


updateCamera'IO :: Env -> IO Camera
updateCamera'IO env = do

  let inp = input $ cWindow env
  mouseInfo <- readIORef $ I.mouses inp
  keysDown  <- readIORef $ I.keys   inp

  let c = updateCamera keysDown (deltaTime env * 5) $ envCamera env
  let w = handler $ cWindow env

  mouseDown <- GLFW.getMouseButton w GLFW.MouseButton'1
  case mouseDown of
    GLFW.MouseButtonState'Pressed  -> do
      GLFW.setCursorInputMode w GLFW.CursorInputMode'Disabled
      return $ c { cameraFront = frontVec mouseInfo }
    GLFW.MouseButtonState'Released -> do
      GLFW.setCursorInputMode w GLFW.CursorInputMode'Normal
      return c

mainEnv :: IO (Maybe Env)
mainEnv = do
  m <- makeWindow $ VideoMode 1080 720 "Demo"
  contextualizeWindow m


onFinish :: Env -> IO ()
onFinish env = do
  close $ cShader env
  close $ vbo env
  -- close $ ebo env
  deleteVao $ vao env
  destroy $ cWindow env

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
    0 0

contextualEnv :: Maybe Env -> IO ()
contextualEnv Nothing = return ()
contextualEnv (Just env) = do
  closeCallback $ cWindow env
  A.run env mainHandler onFinish

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
