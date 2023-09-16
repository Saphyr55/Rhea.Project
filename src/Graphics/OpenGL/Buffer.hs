{-# LANGUAGE InstanceSigs #-}

module Graphics.OpenGL.Buffer where

import Graphics.GL
import Foreign
import Core.Closeable
import GHC.TypeLits

data Buffer = 
    VertexBuffer (Ptr GLuint) 
  | ElementBuffer (Ptr GLuint)

makeVertexBuffer :: [Float] -> IO Buffer
makeVertexBuffer v = do
  let vSize = fromIntegral $ sizeOf (0 :: GLfloat) * length v
  vPtr <- newArray v
  ptr <- makeBufferPtr GL_ARRAY_BUFFER vSize vPtr
  return $ VertexBuffer ptr

makeElementBuffer :: (Integral a) => [a] -> IO Buffer
makeElementBuffer indices = do
  let newIndices = map (\n -> fromIntegral n :: GLuint) indices
  let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * length newIndices
  indicesPtr <- newArray newIndices
  ptr <- makeBufferPtr GL_ELEMENT_ARRAY_BUFFER indicesSize indicesPtr
  return $ ElementBuffer ptr

bindBuffer :: Buffer -> IO ()
bindBuffer (VertexBuffer vbo) = do
  value <- peek vbo
  glBindBuffer GL_ARRAY_BUFFER value

bindBuffer (ElementBuffer ebo) = do
  value <- peek ebo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER value

unbindBuffer :: Buffer -> IO ()
unbindBuffer (ElementBuffer _) =
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

unbindBuffer  (VertexBuffer _) =
  glBindBuffer GL_ARRAY_BUFFER 0

makeBufferPtr :: GLenum -> GLsizeiptr -> Ptr a -> IO (Ptr GLuint)
makeBufferPtr enum len listPtr = do
  bufPtr <- malloc
  glGenBuffers 1 bufPtr
  buf <- peek bufPtr
  glBindBuffer enum buf
  glBufferData enum len listPtr GL_STATIC_DRAW
  return bufPtr

linkBuffer :: Buffer -> Nat -> Nat -> Nat -> Int -> IO ()
linkBuffer buf layout numComp stride offset = do
  bindBuffer buf
  offsetPtr <- new offset
  glVertexAttribPointer
    (fromIntegral layout)
    (fromIntegral numComp)
    GL_FLOAT
    GL_FALSE
    (fromIntegral stride)
    offsetPtr
  glEnableVertexAttribArray
    (fromIntegral layout)
  unbindBuffer buf

instance Closeable Buffer where
  
  close :: Buffer -> IO ()
  close (ElementBuffer ebo) = glDeleteBuffers 1 ebo
  close (VertexBuffer vbo)  = glDeleteBuffers 1 vbo