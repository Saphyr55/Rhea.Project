module Rhea.Graphics.Rendering.Texture (Texture(..)) where

import Foreign
import Rhea.Graphics.Image (ImageInfo)
import Graphics.GL

data Texture = Texture 
  { textureIdPtr  :: Ptr Word32
  , textureId     :: Word32
  , textureSlot   :: Word32
  , textureWidth  :: Word32
  , textureHeight :: Word32
  , textureUnit   :: Word32
  , textureFormat :: Word32
  } deriving ( Show )

createTexture :: ImageInfo -> IO ()
createTexture ii = do
  texturePtr <- malloc
  glGenTextures 1 texturePtr
  texture <- peek texturePtr 
  return ()