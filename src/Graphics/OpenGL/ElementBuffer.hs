module Graphics.OpenGL.ElementBuffer
  ( ElementBufferObject,
    makeEbo,
    deleteEbo,
    bindEbo,
    unbindEbo,
    linkEbo,
    linkEbo'offset
  ) where

import Graphics.GL
import Foreign
import GHC.TypeLits

newtype ElementBufferObject = ElementBufferObject
  (Ptr GLuint)

makeEbo :: Integral a => [a] -> IO ElementBufferObject
makeEbo indices = do
  let newIndices = map ( \n -> fromIntegral n :: GLuint ) indices
  let indicesSize = fromIntegral $ sizeOf (0 :: GLfloat) * length newIndices
  indicesPtr <- newArray newIndices
  eboPtr <- malloc
  glGenBuffers 1 eboPtr
  ebo <- peek eboPtr
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize indicesPtr GL_STATIC_DRAW
  return $ ElementBufferObject eboPtr

bindEbo :: ElementBufferObject -> IO ()
bindEbo (ElementBufferObject ebo) = do
  ebo_ <- peek ebo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo_

unbindEbo :: IO ()
unbindEbo = glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

deleteEbo :: ElementBufferObject -> IO ()
deleteEbo (ElementBufferObject ebo) =
  glDeleteBuffers 1 ebo

linkEbo :: ElementBufferObject -> Nat -> Nat -> Nat -> Ptr a -> IO ()
linkEbo ebo layout numComp stride offset = do
  bindEbo ebo
  glVertexAttribPointer
    (fromIntegral layout)
    (fromIntegral numComp)
    GL_FLOAT
    GL_FALSE
    (fromIntegral stride)
    offset
  glEnableVertexAttribArray (fromIntegral layout)
  unbindEbo

linkEbo'offset :: ElementBufferObject -> Nat -> Nat -> Nat -> IO ()
linkEbo'offset ebo layout numComp stride =
  linkEbo ebo layout numComp stride nullPtr
