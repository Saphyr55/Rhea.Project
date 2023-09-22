module Rhea.Graphics.OpenGL.Texture 
  ( makeTexture
  , bindTexture
  , unbindTexture
  ) where

import Rhea.Graphics.Image (ImageInfo(..))
import Graphics.GL
import Foreign
import Data.Vector.Storable

makeTexture :: ImageInfo -> IO Word32
makeTexture imageInfo = do
  texture <- genTexture
  defaultTexParameteri
  texImage2D imageInfo
  generateMipmap
  return texture

generateMipmap :: IO ()
generateMipmap =
  glGenerateMipmap GL_TEXTURE_2D

texImage2D :: ImageInfo -> IO () 
texImage2D (ImageInfo w h iData) = do 
  unsafeWith iData $ \dataP ->
    glTexImage2D 
      GL_TEXTURE_2D 
      0 
      (fromIntegral GL_RGB) 
      (fromIntegral w) 
      (fromIntegral h) 
      0 
      GL_RGB 
      GL_UNSIGNED_BYTE 
      (castPtr dataP)

genTexture :: IO Word32
genTexture = do
  texturePtr <- malloc
  glGenTextures 1 texturePtr
  texture <- peek texturePtr
  glBindTexture GL_TEXTURE_2D texture
  return texture

bindTexture :: Word32 -> IO ()
bindTexture t = do
  glActiveTexture (GL_TEXTURE0 + 0)
  glBindTexture GL_TEXTURE_2D t

unbindTexture :: IO ()
unbindTexture = 
  bindTexture 0

defaultTexParameteri :: IO ()
defaultTexParameteri = do
  glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_WRAP_S       (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_WRAP_T       (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_MIN_FILTER   (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri GL_TEXTURE_2D   GL_TEXTURE_MAG_FILTER   (fromIntegral GL_LINEAR)
