module Rhea.Graphics.OpenGL.Uniform where

import Rhea.Math.Vector
import Rhea.Graphics.Rendering.ShaderType (Uniform (..))

uniform'WithNoUseShader :: String -> IO (Uniform a)
uniform'WithNoUseShader name = 
  withCString name $ \n -> do
    location <-
      glGetUniformLocation
        $= handler shader
        $ castPtr n
    return $ Uniform $ fromIntegral location

withCString :: String -> (t7 -> m0 (Uniform a2)) -> IO (Uniform a)
withCString = _

updateUniform'WithNoUseShader :: Uniform a -> a -> IO ()
updateUniform'WithNoUseShader (Uniform h) (Vector3 x y z) = glUniform3f (fromIntegral h) x y z
updateUniform'WithNoUseShader (Uniform h) (Vector4 x y z w) = glUniform4f (fromIntegral h) x y z w
