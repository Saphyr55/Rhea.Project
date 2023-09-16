module Graphics.OpenGL.VertexArray 
  (
    VertexArrayObject,
    makeVao, 
    bindVao, 
    unbindVao, 
    deleteVao
  ) where

import Foreign
import Graphics.GL

newtype VertexArrayObject = VertexArrayObject 
  (Ptr GLuint)

makeVao :: IO VertexArrayObject
makeVao = do
  vaoPtr <- malloc
  glGenVertexArrays 1 vaoPtr
  vao <- peek vaoPtr
  glBindVertexArray vao
  return $ VertexArrayObject vaoPtr

bindVao :: VertexArrayObject -> IO ()
bindVao (VertexArrayObject vao) = do
  value <- peek vao
  glBindVertexArray value

unbindVao :: IO ()
unbindVao =
  glBindVertexArray 0

deleteVao :: VertexArrayObject -> IO ()
deleteVao  (VertexArrayObject vao) =
  glDeleteVertexArrays 1 vao