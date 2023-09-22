module Rhea.Graphics.OpenGL.Shader
  (
    createShader,
    useShader,
    close,
  ) where

import Rhea.Graphics.Rendering.ShaderType
import Graphics.GL
import Foreign
import Foreign.C.String
import Control.Monad

data ShaderPart = ShaderPart Int ShaderType

createShader :: [ShaderInfo] -> IO Shader
createShader infos = do
  parts <- mapM makeShaderPart infos
  prog <- makeProgram parts
  useShader prog
  deleteShaders parts
  return prog

useShader :: Shader -> IO ()
useShader (GLShader s) =
  glUseProgram $ fromIntegral s

close :: Shader -> IO ()
close (GLShader s) =
    glDeleteProgram $ fromIntegral s

makeProgram :: [ShaderPart] -> IO Shader
makeProgram parts = do
  prog <- glCreateProgram
  attachShader prog parts
  glLinkProgram prog
  let p = GLShader $ fromIntegral prog
  checkProgram p
  return p

attachShader :: GLuint -> [ShaderPart] -> IO ()
attachShader prog parts =
  forM_ parts $ \(ShaderPart h _) ->
    glAttachShader prog $ fromIntegral h

makeShaderPart :: ShaderInfo -> IO ShaderPart
makeShaderPart (typ, source) = do
  vs <- glCreateShader $ toGLType typ
  let shader = ShaderPart (fromIntegral vs) typ
  shaderSource source shader
  compileShader shader
  checkShader shader
  return shader

deleteShaders :: [ShaderPart] -> IO ()
deleteShaders parts =
  forM_ parts $ \(ShaderPart h _) ->
    glDeleteShader $ fromIntegral h

toGLType :: ShaderType -> GLuint
toGLType FragmentShader = GL_FRAGMENT_SHADER
toGLType VertexShader   = GL_VERTEX_SHADER

shaderSource :: String -> ShaderPart -> IO ()
shaderSource src (ShaderPart h _) = do
  (sourceP, len) <- newCAStringLen src
  linesPtrsPtr <- newArray [sourceP]
  lengthsPtr <- newArray [fromIntegral len]
  glShaderSource (fromIntegral h) 1 linesPtrsPtr lengthsPtr

compileShader :: ShaderPart -> IO ()
compileShader (ShaderPart h _) =
  glCompileShader $ fromIntegral h

checkShader :: ShaderPart -> IO ()
checkShader (ShaderPart h _) = do
  successPtr <- malloc
  glGetShaderiv (fromIntegral h) GL_COMPILE_STATUS successPtr
  success <- peek successPtr
  when (success == 0) $ do
    logErrorShader $ fromIntegral h

checkProgram :: Shader -> IO ()
checkProgram (GLShader shader) = do
  successPtr <- malloc
  glGetProgramiv (fromIntegral shader) GL_LINK_STATUS successPtr
  success <- peek successPtr
  when (success == 0) $ do
    logErrorShader $ fromIntegral shader

logErrorShader :: GLuint -> IO ()
logErrorShader shader = do
  infoLogPtr <- malloc
  glGetShaderInfoLog shader 512 nullPtr infoLogPtr
  infoLog <- peekCString infoLogPtr
  putStrLn infoLog
