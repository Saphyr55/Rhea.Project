module Core.Common where

identity :: a -> a
identity a = a

($=) :: (a -> b) -> a -> b
($=) f = f