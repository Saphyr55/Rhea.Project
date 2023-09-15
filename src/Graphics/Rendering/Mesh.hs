module Graphics.Rendering.Mesh where

import Graphics.Rendering.OpenGL 
    ( Vector3, Vector2, Color3 )
import GHC.TypeLits 
    ( Nat )

data Vertex = Vertex
  { position :: Vector3 Float,
    normal   :: Vector3 Float,
    color    :: Color3 Float,
    texCoord :: Vector2 Float }

data Mesh = Mesh
  { verticies   :: [Vertex], 
    indices     :: [Nat] }