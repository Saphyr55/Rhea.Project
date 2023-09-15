module Graphics.OpenGL.OpenGL where

import Graphics.GL (GLfloat, GLint, GLint64)
import Foreign (Int32, Int64)

mapGlfloat :: Float -> GLfloat
mapGlfloat n = n

mapGlint32 :: Int32 -> GLint
mapGlint32 n = n

mapGlint64 :: Int64 -> GLint64
mapGlint64 n = n