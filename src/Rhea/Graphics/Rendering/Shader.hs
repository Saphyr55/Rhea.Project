{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Rhea.Graphics.Rendering.Shader 
  ( createShader,
    useShader,
    uniform,
    close
  ) where

import Rhea.Graphics.Rendering.Uniform (Uniform)
import qualified Rhea.Graphics.OpenGL.Shader as GL
import Rhea.Graphics.Rendering.ShaderType
import Rhea.Core.Closeable

createShader :: [ShaderInfo] -> IO Shader
createShader = GL.createShader

useShader :: Shader -> IO ()
useShader = GL.useShader

uniform :: Shader -> String -> IO (Uniform a)
uniform = GL.uniform

instance Closeable Shader where

  close :: Shader -> IO ()
  close = GL.close
