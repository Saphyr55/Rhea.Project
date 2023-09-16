module Graphics.Rendering.Shader where

data ShaderType =
    VertexShader
  | FragmentShader

type ShaderInfo = (ShaderType, String)

class Shader t where

  makeShader :: [ShaderInfo] -> IO t

  use :: t -> IO ()