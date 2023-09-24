{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rhea.Graphics.Rendering.Shader
  ( createShader
  , useShader
  , uniform
  , close
  , updateUniform
  , defaultShader
  ) where

import qualified Rhea.Graphics.OpenGL.Shader as GL
import qualified Rhea.Graphics.OpenGL.Uniform as U
import Rhea.Graphics.Rendering.ShaderType
import Rhea.Core.Closeable
import Rhea.Core.Resources

defaultShader :: IO Shader
defaultShader = do
  resVert <- readResource "/Shaders/Default.vert"
  resFrag <- readResource "/Shaders/Default.frag"
  createShader
    [ (VertexShader, resVert),
      (FragmentShader, resFrag)
    ]

createShader :: [ShaderInfo] -> IO Shader
createShader = GL.createShader

useShader :: Shader -> IO ()
useShader = GL.useShader

uniform :: Shader -> String -> IO UniformLocation
uniform shader name = do
  useShader shader
  U.uniform' shader name

updateUniform'Location :: UniformLocation -> Uniform -> IO ()
updateUniform'Location = U.updateUniform'

updateUniform :: Shader -> Uniform -> IO ()
updateUniform shader uni@(Uniform3f name _) = updateUniform' shader name uni
updateUniform shader uni@(Uniform4f name _) = updateUniform' shader name uni
updateUniform shader uni@(Uniform1i name _) = updateUniform' shader name uni
updateUniform shader uni@(UniformMatrix4f name _) = updateUniform' shader name uni

updateUniform' :: Shader -> String -> Uniform -> IO ()
updateUniform' shader name uni = do
  location <- uniform shader name
  updateUniform'Location location uni

instance Closeable Shader where

  close :: Shader -> IO ()
  close = GL.close

