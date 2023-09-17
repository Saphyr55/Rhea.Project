{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Rendering.Uniform 
  ( Uniform (..),
    UniformSystem (..)
  ) where
  
import Math.Vector
import Graphics.GL
import GHC.TypeLits

newtype Uniform a = Uniform Nat

class UniformSystem t where

  updateUniform :: Uniform t -> t -> IO ()

instance UniformSystem Vector3f where

  updateUniform :: Uniform Vector3f -> Vector3f -> IO ()
  updateUniform (Uniform h) (Vector3 x y z) = 
    glUniform3f (fromIntegral h) x y z

instance UniformSystem Vector4f where

  updateUniform :: Uniform Vector4f -> Vector4f -> IO ()
  updateUniform (Uniform h) (Vector4 x y z w) =
    glUniform4f (fromIntegral h) x y z w