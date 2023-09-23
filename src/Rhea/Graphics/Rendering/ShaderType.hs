module Rhea.Graphics.Rendering.ShaderType 
  ( ShaderType(..),
    Shader(..),
    ShaderInfo,
    UniformLocation(..),
    Uniform(..)
  ) where

import Linear
import Data.Word

data ShaderType
  = VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

newtype Shader = GLShader Int

newtype UniformLocation = UniformLocation Word64

data Uniform
  = Uniform3f String (V3 Float)
  | Uniform4f String (V4 Float)
  | Uniform1i String Int
  | UniformMatrix4f String (M44 Float)