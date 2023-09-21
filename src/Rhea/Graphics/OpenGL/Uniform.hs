module Rhea.Graphics.OpenGL.Uniform where

import Rhea.Graphics.Rendering.ShaderType
import Foreign.C
import Rhea.Core.Common
import Graphics.GL
import Foreign
import Rhea.Math.Vector

uniform' :: Shader -> String -> IO UniformLocation
uniform' (GLShader shader) name =
  withCString name $ \cname -> do
    location <-
      glGetUniformLocation 
        $= fromIntegral shader 
        $ castPtr cname
    return $ UniformLocation $ fromIntegral location

updateUniform' :: UniformLocation -> Uniform -> IO ()
updateUniform' (UniformLocation u) (Uniform4f _ (Vector4 x y z w)) = 
  let location = fromIntegral u in
  glUniform4f location x y z w
updateUniform' (UniformLocation u) (Uniform3f _ (Vector3 x y z)) = 
  let location = fromIntegral u in
  glUniform3f location x y z
