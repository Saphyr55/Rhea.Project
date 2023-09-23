module Rhea.Core.Common (($=)) where

($=) :: (a -> b) -> a -> b
($=) f = f