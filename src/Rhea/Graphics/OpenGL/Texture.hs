module Rhea.Graphics.OpenGL.Texture 
  ( 
    
  ) where

import Graphics.GL
import Foreign (malloc, peek)
import Data.Word (Word64)
import Codec.Picture
import Rhea.Core.Resources

image = do
    filepath <-  resourceFilepath "Textures/wall.jpg"
    eErrDI <- readImage filepath
    dyImage <- case eErrDI of
        Left e -> do
            putStrLn e
            return $ ImageRGB8 $ generateImage (\x _ ->
                let x' = fromIntegral x in PixelRGB8 x' x' x') 800 600
        Right di -> return di
    let ipixelrgb8 = convertRGB8 dyImage
    let d = imageData ipixelrgb8
    return (imageWidth ipixelrgb8, imageHeight ipixelrgb8, d)

genTexture :: IO Word64
genTexture = do
    texturePtr <- malloc
    glGenTextures 1 texturePtr
    texture <- peek texturePtr
    glBindTexture GL_TEXTURE_2D texture
    return $ fromIntegral texture
