module Rhea.Graphics.OpenGL.Uniform (uniform', updateUniform') where

import Rhea.Graphics.Rendering.ShaderType
import Rhea.Core.Common
import Graphics.GL
import Foreign
import Foreign.C
import Linear

uniform' :: Shader -> String -> IO UniformLocation
uniform' (GLShader shader) name =
  withCString name $ \cname -> do
    location <-
      glGetUniformLocation 
        $= fromIntegral shader 
        $ castPtr cname
    return $ UniformLocation $ fromIntegral location

updateUniform' :: UniformLocation -> Uniform -> IO ()
updateUniform' (UniformLocation u) (Uniform4f _ (V4 x y z w)) = glUniform4f (fromIntegral u) x y z w
updateUniform' (UniformLocation u) (Uniform3f _ (V3 x y z))   = glUniform3f (fromIntegral u) x y z
updateUniform' (UniformLocation u) (Uniform1i _ i)            = glUniform1i $= fromIntegral u $ fromIntegral i
updateUniform' (UniformLocation u) (UniformMatrix4f _ m) = do
  mPtr <- malloc
  poke mPtr (transpose m)
  glUniformMatrix4fv (fromIntegral u) 1 GL_FALSE $ castPtr mPtr

