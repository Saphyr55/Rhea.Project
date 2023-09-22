module Rhea.Graphics.Image 
  ( ImageInfo(..)
  , image
  ) where

import Data.Word ( Word8, Word32 )
import qualified Data.Vector.Storable as VS
import Rhea.Core.Resources
import Codec.Picture

data ImageInfo = ImageInfo 
  Word32 Word32 (VS.Vector Word8)

image :: String -> IO ImageInfo
image source = do
  filepath <- resourceFilepath source
  eErrDI   <- readImage filepath
  dyImage  <- case eErrDI of
    Left e -> errorImage e
    Right di -> return di
  let ipixelrgb8 = convertRGB8 dyImage
  return $ ImageInfo
    (fromIntegral $ imageWidth ipixelrgb8)
    (fromIntegral $ imageHeight ipixelrgb8)
    (imageData ipixelrgb8)

errorImage :: String -> IO DynamicImage
errorImage e = do
  putStrLn e
  return $ ImageRGB8 $
    generateImage 
      (\x _ -> let x' = fromIntegral x in PixelRGB8 x' x' x') 
      800 600