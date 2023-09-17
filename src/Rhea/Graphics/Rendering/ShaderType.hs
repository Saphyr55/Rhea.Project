module Rhea.Graphics.Rendering.ShaderType 
  ( ShaderType(..),
    Shader(..),
    ShaderInfo,
  ) where

data ShaderType
  = VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

newtype Shader = GLShader Int
