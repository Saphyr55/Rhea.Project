module Rhea.Graphics.Rendering.ShaderType 
  ( ShaderType(..),
    Shader(..),
    ShaderInfo,
    UniformLocation(..),
    Uniform(..)
  ) where

import GHC.TypeLits
import Rhea.Math.Vector

data ShaderType
  = VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

newtype Shader = GLShader Int

newtype UniformLocation = UniformLocation Nat

data Uniform
  = Uniform3f String Vector3f
  | Uniform4f String Vector4f
