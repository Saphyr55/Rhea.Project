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

data VertexArrayObject = VertexArrayObject
  { handler :: Ptr GLuint}

makeVao :: IO VertexArrayObject
makeVao = do
  vaoPtr <- malloc
  glGenVertexArrays 1 vaoPtr
  vao <- peek vaoPtr
  glBindVertexArray vao
  return $ VertexArrayObject vaoPtr

bindVao :: VertexArrayObject -> IO ()
bindVao vao = do
  value <- peek $ handler vao
  glBindVertexArray value

unbindVao :: IO ()
unbindVao =
  glBindVertexArray 0

deleteVao :: VertexArrayObject -> IO ()
deleteVao vao =
  glDeleteVertexArrays 1 $ handler vao