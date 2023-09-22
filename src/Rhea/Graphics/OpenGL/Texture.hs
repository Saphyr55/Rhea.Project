module Rhea.Graphics.OpenGL.Texture 
  ( makeTexture
  , bindTexture
  , unbindTexture
  ) where

import Rhea.Graphics.Image (ImageInfo(..))
import Graphics.GL
import Foreign (malloc, peek)
import Data.Word (Word64)
import qualified Data.Vector.Storable as VS

makeTexture :: ImageInfo -> IO Word64
makeTexture imageInfo = do
    texture <- genTexture
    defaultTexParameteri
    texImage2D imageInfo
    generateMipmap
    unbindTexture
    return texture

generateMipmap :: IO ()
generateMipmap =
    glGenerateMipmap GL_TEXTURE_2D

texImage2D :: ImageInfo -> IO ()
texImage2D (ImageInfo w h iData) = 
    VS.unsafeWith iData $ \dataPtr ->
        glTexImage2D
            GL_TEXTURE_2D 
            0 
            (fromIntegral GL_RGB)
            (fromIntegral w) 
            (fromIntegral h) 
            0 
            GL_RGB 
            GL_UNSIGNED_BYTE 
            dataPtr

genTexture :: IO Word64
genTexture = do
    texturePtr <- malloc
    glGenTextures 1 texturePtr
    texture <- peek texturePtr
    bindTexture $ fromIntegral texture
    return $ fromIntegral texture

bindTexture :: Word64 -> IO ()
bindTexture texture = 
    glBindTexture GL_TEXTURE_2D $ fromIntegral texture

unbindTexture :: IO ()
unbindTexture = 
    bindTexture 0

defaultTexParameteri :: IO ()
defaultTexParameteri = do
    glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_WRAP_S       (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_WRAP_T       (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_MIN_FILTER   (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
    glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_MAG_FILTER   (fromIntegral GL_LINEAR)
