module Graphics.Color ( Color(..), rgba, red, green, blue, alpha ) where

type RGBA = (Float, Float, Float, Float)

data Color = 
    Color RGBA
  | Red
  | Blue
  | Green 
  | White
  | Black
  | Charcoal
  deriving ( Show, Eq )

rgba :: Color -> RGBA
rgba color = case color of
  Red -> (1.0, 0.0, 0.0, 1.0)
  Green -> (0.0, 1.0, 0.0, 1.0)
  Blue -> (0.0, 0.0, 1.0, 1.0)
  White -> (1.0, 1.0, 1.0, 1.0)
  Black -> (0.0, 0.0, 0.0, 1.0)
  Charcoal -> (0.21, 0.27, 0.30, 1.0)
  Color rgba_ -> rgba_

red :: Color -> Float
red color = case rgba color of (r, _, _, _) -> r

green :: Color -> Float
green color = case rgba color of (_, g, _, _) -> g

blue :: Color -> Float
blue color = case rgba color of (_, _, b, _) -> b

alpha :: Color -> Float
alpha color = case rgba color of (_, _, _, a) -> a

