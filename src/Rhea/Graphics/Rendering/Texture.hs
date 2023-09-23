module Rhea.Graphics.Rendering.Texture 
  ( Texture(..)
  , createTexture
  , bindTexture
  , unbindTexture
  ) where

import Foreign
import Rhea.Graphics.Image (ImageInfo (..))
import qualified Rhea.Graphics.OpenGL.Texture as GL

data Texture = Texture 
  { textureId     :: Word32
  , textureSlot   :: Word32
  , textureWidth  :: Word32
  , textureHeight :: Word32
  } deriving ( Show, Eq )

createTexture :: ImageInfo -> Word32 -> IO Texture
createTexture infoImage@(ImageInfo iw ih _) slot = do
  texID <- GL.makeTexture infoImage
  return $ Texture texID slot iw ih

bindTexture :: Texture -> IO ()
bindTexture (Texture i s _ _) = GL.bindTexture i s

unbindTexture :: IO ()
unbindTexture = GL.unbindTexture