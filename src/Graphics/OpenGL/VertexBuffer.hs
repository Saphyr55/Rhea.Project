module Graphics.OpenGL.VertexBuffer 
  (
    VertexBufferObject,
    makeVbo,
    linkVbo,
    bindVbo,
    unbindVbo,
    deleteVbo,
    linkVbo'offset,
  ) where

import Graphics.GL
import Foreign
import GHC.TypeLits (Nat)

newtype VertexBufferObject = VertexBufferObject 
  { handler :: Ptr GLuint }

makeVbo :: [Float] -> IO VertexBufferObject
makeVbo verticies = do
  let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * length verticies
  verticesPtr <- newArray verticies
  vboPtr <- malloc
  glGenBuffers 1 vboPtr
  vbo <- peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesPtr) GL_STATIC_DRAW
  return $ VertexBufferObject vboPtr

linkVbo :: VertexBufferObject -> Nat -> Nat -> Nat -> Ptr a -> IO ()
linkVbo vbo layout numComp stride offset = do
  bindVbo vbo
  glVertexAttribPointer 
    (fromIntegral layout) 
    (fromIntegral numComp) 
    GL_FLOAT
    GL_FALSE 
    (fromIntegral stride) 
    offset
  glEnableVertexAttribArray 
    (fromIntegral layout)
  unbindVbo

linkVbo'offset :: VertexBufferObject -> Nat -> Nat -> Nat -> IO ()
linkVbo'offset vbo layout numComp stride = 
  linkVbo vbo layout numComp stride nullPtr

bindVbo :: VertexBufferObject -> IO ()
bindVbo vbo = do
  value <- peek $ handler vbo
  glBindBuffer GL_ARRAY_BUFFER value

unbindVbo :: IO ()
unbindVbo = 
  glBindBuffer GL_ARRAY_BUFFER 0

deleteVbo :: VertexBufferObject -> IO ()
deleteVbo vbo =
  glDeleteBuffers 1 $ handler vbo
