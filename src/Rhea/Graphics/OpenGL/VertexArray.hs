module Rhea.Graphics.OpenGL.VertexArray 
  (
    VertexArray,
    makeVertexArray, 
    bindVao, 
    unbindVao, 
    deleteVao
  ) where

import Foreign
import Graphics.GL

newtype VertexArray = VertexArray
  (Ptr GLuint)

makeVertexArray :: IO VertexArray
makeVertexArray = do
  vaoPtr <- malloc
  glGenVertexArrays 1 vaoPtr
  vao <- peek vaoPtr
  glBindVertexArray vao
  return $ VertexArray vaoPtr

bindVao :: VertexArray -> IO ()
bindVao (VertexArray vao) = do
  value <- peek vao
  glBindVertexArray value

unbindVao :: IO ()
unbindVao =
  glBindVertexArray 0

deleteVao :: VertexArray -> IO ()
deleteVao  (VertexArray vao) =
  glDeleteVertexArrays 1 vao