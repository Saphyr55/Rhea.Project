{-# LANGUAGE InstanceSigs #-}

module Graphics.OpenGL.Shader
  (
    Shader(..),
    GLShader(..)
  ) where

import Graphics.Rendering.Shader
import Graphics.GL
import Foreign
import Foreign.C.String
import Control.Monad
import Core.Common

newtype GLShader =
  GLShader { handler :: GLuint }

data GLShaderPart = GLShaderPart GLuint ShaderType

instance Shader GLShader where

  makeShader :: [ShaderInfo] -> IO GLShader
  makeShader infos = do
    parts <- mapM makeShaderPart infos
    prog <- makeProgram parts
    useShader prog
    deleteShaders parts
    return prog

  useShader :: GLShader -> IO ()
  useShader s =
    glUseProgram $ handler s

  deleteShader :: GLShader -> IO ()
  deleteShader s =
     glDeleteProgram $ handler s

  uniformLocation :: GLShader -> String -> IO Int
  uniformLocation shader name = do 
    withCString name $ 
      \n -> do 
        location <- glGetUniformLocation 
          $= handler shader 
          $ castPtr n
        return $ fromIntegral location

makeProgram :: [GLShaderPart] -> IO GLShader
makeProgram parts = do
  prog <- glCreateProgram
  attachShader prog parts
  glLinkProgram prog
  let p = GLShader prog
  checkProgram p

  return p

attachShader :: GLuint -> [GLShaderPart] -> IO ()
attachShader prog parts =
    forM_ parts $ \(GLShaderPart h _) ->
      glAttachShader prog h

makeShaderPart :: ShaderInfo -> IO GLShaderPart
makeShaderPart (typ, source) = do
  vs <- glCreateShader $ toGLType typ
  let shader = GLShaderPart vs typ
  shaderSource source shader
  compileShader shader
  checkShader shader
  return shader

deleteShaders :: [GLShaderPart] -> IO ()
deleteShaders parts =
  forM_ parts $ \(GLShaderPart h _) ->
    glDeleteShader h

toGLType :: ShaderType -> GLuint
toGLType FragmentShader = GL_FRAGMENT_SHADER
toGLType VertexShader   = GL_VERTEX_SHADER

shaderSource :: String -> GLShaderPart -> IO ()
shaderSource src (GLShaderPart h _) = do
  (sourceP, len) <- newCAStringLen src
  linesPtrsPtr <- newArray [sourceP]
  lengthsPtr <- newArray [fromIntegral len]
  glShaderSource h 1 linesPtrsPtr lengthsPtr

compileShader :: GLShaderPart -> IO ()
compileShader (GLShaderPart h _) =
  glCompileShader h

checkShader :: GLShaderPart -> IO ()
checkShader (GLShaderPart h _) = do
  successPtr <- malloc
  glGetShaderiv h GL_COMPILE_STATUS successPtr
  success <- peek successPtr
  when (success == 0) $ do
    logErrorShader h

checkProgram :: GLShader -> IO ()
checkProgram shader = do
  successPtr <- malloc
  glGetProgramiv (handler shader) GL_LINK_STATUS successPtr
  success <- peek successPtr
  when (success == 0) $ do
    logErrorShader $ handler shader

logErrorShader :: GLuint -> IO ()
logErrorShader shader = do
  infoLogPtr <- malloc
  glGetShaderInfoLog shader 512 nullPtr infoLogPtr
  infoLog <- peekCString infoLogPtr
  putStrLn infoLog