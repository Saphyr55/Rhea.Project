{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Rhea.Graphics.Rendering.Shader 
  ( createShader,
    useShader,
    uniform,
    close
  ) where

import qualified Rhea.Graphics.OpenGL.Shader as GL
import qualified Rhea.Graphics.OpenGL.Uniform as U
import Rhea.Graphics.Rendering.ShaderType
import Rhea.Core.Closeable

createShader :: [ShaderInfo] -> IO Shader
createShader = GL.createShader

useShader :: Shader -> IO ()
useShader = GL.useShader

uniform :: Shader -> String -> IO (Uniform a)
uniform s = useShader s
  U.uniform'WithNoUseShader
 
updateUniform :: Shader -> Uniform a -> a -> IO ()
updateUniform s = useShader s
  U.updateUniform'WithNoUseShader 

instance Closeable Shader where

  close :: Shader -> IO ()
  close = GL.close
