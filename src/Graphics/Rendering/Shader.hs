module Graphics.Rendering.Shader 
  ( Shader (..),
    ShaderType (..),
    ShaderInfo
  ) where

data ShaderType =
    VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

class Shader t where

  makeShader :: [ShaderInfo] -> IO t

  useShader :: t -> IO ()

  deleteShader :: t -> IO ()