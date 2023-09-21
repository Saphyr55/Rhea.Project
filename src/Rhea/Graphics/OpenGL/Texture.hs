module Rhea.Graphics.OpenGL.Texture where

import Graphics.GL
import Foreign (malloc, peek)
import Data.Word (Word64)

genTexture :: IO Word64
genTexture = do
    texturePtr <- malloc
    glGenTextures 1 texturePtr
    texture <- peek texturePtr
    glBindTexture GL_TEXTURE_2D texture
    return $ fromIntegral texture
