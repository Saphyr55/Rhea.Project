module Rhea.Graphics.Rendering.ShaderType 
  ( ShaderType(..),
    Shader(..),
    ShaderInfo,
    UniformLocation(..),
    Uniform(..)
  ) where

import GHC.TypeLits
import Linear

data ShaderType
  = VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

newtype Shader = GLShader Int

newtype UniformLocation = UniformLocation Nat

data Uniform
  = Uniform3f String (V3 Float)
  | Uniform4f String (V4 Float)
