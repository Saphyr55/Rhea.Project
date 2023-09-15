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

data Context = Context
  { window :: Window,
    renderer :: GLRenderer,
    voa :: GLuint }

genSomething :: IO GLuint
genSomething = do
  let verticies = [
        -0.5, -0.5, 0.0, -- first vertex
         0.5, -0.5, 0.0, -- second vertex
         0.0,  0.5, 0.0 -- third vertex
        ] :: [GLfloat]
  let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * length verticies
  verticesPtr <- newArray verticies
  vboPtr <- malloc
  glGenBuffers 1 vboPtr
  vbo <- peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesPtr) GL_STATIC_DRAW
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesPtr) GL_STATIC_DRAW
  let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
  glEnableVertexAttribArray 0
  return vao

videoMode :: VideoMode
videoMode = VideoMode 1080 720 "Demo"

mainHandler :: Context -> IO Context
mainHandler context = do
  
  clearColor $= renderer context $ Charcoal
  clear $ renderer context

  glBindVertexArray $ voa context
  glDrawArrays GL_TRIANGLES 0 3
  glBindVertexArray 0

  swap $ window context
  return context

mainContext :: IO (Maybe Context)
mainContext = do
  m <- makeWindow videoMode
  contextualizeWindow m

onFinish :: Context -> IO ()
onFinish context = do
  destroy $ window context

contextualizeWindow :: Maybe Window -> IO (Maybe Context)
contextualizeWindow Nothing = return Nothing
contextualizeWindow (Just w) = do
  Just . Context w GLRenderer <$> genSomething

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