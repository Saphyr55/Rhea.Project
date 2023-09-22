{-# LANGUAGE InstanceSigs #-}

module Rhea.Graphics.OpenGL.Buffer
  ( Buffer(..),
    makeVertexBuffer,
    makeElementBuffer,
    bindBuffer,
    unbindBuffer,
    linkBuffer,
    close,
  ) where

import Graphics.GL
import Data.Word ( Word64 )
import Foreign ( Ptr, castPtr, malloc, newArray, Storable(peek, sizeOf), nullPtr )
import Rhea.Core.Closeable

data Buffer = 
    VertexBuffer (Ptr GLuint)
  | ElementBuffer (Ptr GLuint)

instance Closeable Buffer where

  close :: Buffer -> IO ()
  close (ElementBuffer ebo) = glDeleteBuffers 1 ebo
  close (VertexBuffer vbo)  = glDeleteBuffers 1 vbo

makeVertexBuffer :: [Float] -> IO Buffer
makeVertexBuffer verticies = do
  let vSize = fromIntegral $ sizeOf (0 :: GLfloat) * length verticies 
  vPtr <- newArray verticies 
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
  glBufferData enum len (castPtr listPtr) GL_STATIC_DRAW
  return bufPtr

linkBuffer :: Buffer -> Word64 -> Word64 -> Word64 -> IO ()
linkBuffer buf layout numComp stride = do
  bindBuffer buf
  glVertexAttribPointer
    (fromIntegral layout)
    (fromIntegral numComp)
    GL_FLOAT
    GL_FALSE
    (fromIntegral stride)
    nullPtr
  glEnableVertexAttribArray 
    $ fromIntegral layout
  unbindBuffer buf