module Rhea.Graphics.Rendering.ShaderType 
  ( ShaderType(..),
    Shader(..),
    ShaderInfo,
    Uniform(..)
  ) where
import GHC.TypeLits

data ShaderType
  = VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

newtype Shader = GLShader Int

newtype Uniform a = Uniform Nat